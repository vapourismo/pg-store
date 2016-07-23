{-# LANGUAGE TemplateHaskell, BangPatterns #-}

-- |
-- Module:     Database.PostgreSQL.Store.Table
-- Copyright:  (c) Ole Krüger 2015-2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.Table (
	-- * Auxiliary data types
	Reference (..),

	-- * Table types
	Table (..),
	mkCreateQuery,

	-- * Table generation
	TableConstraint (..),
	mkTable
) where

import Control.Monad
import Control.Monad.Except

import Data.Int
import Data.List hiding (insert)
import Data.Proxy
import Data.String

import qualified Data.ByteString as B

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Database.PostgreSQL.Store.Query
import Database.PostgreSQL.Store.Columns
import Database.PostgreSQL.Store.Result
import Database.PostgreSQL.Store.Errand

-- | Reference a row of type @a@.
newtype Reference a = Reference { referenceID :: Int64 }
	deriving (Eq, Ord)

instance Show (Reference a) where
	show (Reference n) = show n

instance (QueryTable a) => Column (Reference a) where
	pack ref = pack (referenceID ref)

	unpack val = Reference <$> unpack val

	columnTypeName proxy =
		"BIGINT REFERENCES " ++ quoteIdentifier (tableName tableProxy) ++
		" (" ++ quoteIdentifier (tableIDName tableProxy) ++ ")"
		where
			tableProxy = (const Proxy :: Proxy (Reference a) -> Proxy a) proxy

	columnAllowNull _ = False

instance Result (Reference a) where
	queryResultProcessor = Reference <$> unpackColumn

-- | Qualify @a@ as a table type. 'mkTable' can implement this class for you.
class Table a where
	-- | Insert a row into the table and return a 'Reference' to the inserted row.
	insert :: a -> Errand (Reference a)

	-- | Insert multiple rows into the table at once.
	insertMany :: [a] -> Errand [Reference a]
	insertMany = mapM insert

	-- | Find the row identified by the given reference.
	find :: Reference a -> Errand a

	-- | Update an existing row.
	update :: Reference a -> a -> Errand ()

	-- | Delete a row from the table.
	delete :: Reference a -> Errand ()

	-- | Generate the query which creates this table inside the database.
	-- Use 'mkCreateQuery' for convenience.
	createTableQuery :: Proxy a -> Query

-- | Table field declaration
data TableField = TableField String Type Name

-- | Table type declaration
data TableDec = TableDec Name Name [TableField] Pat

-- | Generate an identifier for a table type.
tableIdentifier :: Name -> String
tableIdentifier name = quoteIdentifier (show name)

-- | Make a comma-seperated list of identifiers which correspond to given fields.
tableFieldIdentifiers :: [TableField] -> String
tableFieldIdentifiers fields =
	intercalate ", " (map (\ (TableField name _ _) -> quoteIdentifier name) fields)

-- | Beginning of a insert statement.
tableInsertStatementBegin :: TableDec -> String
tableInsertStatementBegin (TableDec table _ fields _) =
	"INSERT INTO " ++
	tableIdentifier table ++
	" (" ++
	tableFieldIdentifiers fields ++
	") VALUES "

-- | End of a insert statement.
tableInsertStatementEnd :: String
tableInsertStatementEnd =
	" RETURNING \"$id\""

-- | Bind fields of a table type instance.
bindFields :: Name -> TableDec -> ([Name] -> Q Exp) -> Q Exp
bindFields row (TableDec _ ctor fields _) action =
	mapM (\ (TableField name _ _) -> newName name) fields >>= generate
	where
		generate names =
			-- let Ctor name0 name1 .. nameN = row in $(action ['name0, 'name1 ... 'nameN])
			LetE [destructureCtor names] <$> action names

		destructureCtor names =
			-- Ctor name0 name1 ... nameN = row
			ValD (ConP ctor (map VarP names)) (NormalB (VarE row)) []

-- | Pack fields of a table type instance.
packFields :: Name -> TableDec -> Q Exp
packFields row dec =
	bindFields row dec $ \ names ->
		-- [pack name0, pack name1 ... pack nameN]
		pure (ListE (map (\ n -> AppE (VarE 'pack) (VarE n)) names))

-- | Call a constructor with some variables.
callConstructor :: Name -> [Name] -> Exp
callConstructor ctor params =
	foldl AppE (ConE ctor) (map VarE params)

-- | Generate the list of selectors.
makeQuerySelectors :: [TableField] -> Q Exp
makeQuerySelectors fields =
	ListE <$> mapM (\ (TableField field _ _) -> [e| SelectorField $(stringE field) |]) fields

-- | Generate the result processor for a table.
makeResultProcessor :: TableDec -> Q Exp
makeResultProcessor (TableDec _ ctor fields _) = do
	bindingNames <- mapM (\ (TableField name _ _) -> newName name) fields
	pure (DoE (map makeBinding bindingNames ++
	           [makeConstruction bindingNames]))
	where
		-- Bind expression 'name <- unpackColumn'
		makeBinding name =
			BindS (VarP name) (VarE 'unpackColumn)

		-- Last expression 'pure (ctor boundNames...)'
		makeConstruction names =
			NoBindS (AppE (VarE 'pure) (callConstructor ctor names))

-- | Generate the create query.
makeCreateQuery :: TableDec -> [TableConstraint] -> Q Exp
makeCreateQuery (TableDec table _ fields _) constraints =
	runQueryBuilder $ do
		writeStringCode "CREATE TABLE IF NOT EXISTS " :: QueryBuilder [Q Exp] (Q Exp)
		writeIdentifier (show table)
		writeStringCode " ("

		intercalateBuilder (writeStringCode ", ") $
			identDescription :
			map describeField fields ++
			map describeConstraint constraints

		writeStringCode ")"
	where
		identDescription = do
			writeIdentifier "$id"
			writeStringCode "BIGSERIAL NOT NULL PRIMARY KEY"

		describeField :: TableField -> QueryBuilder [Q Exp] (Q Exp)
		describeField (TableField name typ _) =
			writeCode [e| fromString (columnDescription (Proxy :: Proxy $(pure typ))
			                                            $(stringE (quoteIdentifier name))) |]

		describeConstraint :: TableConstraint -> QueryBuilder [Q Exp] (Q Exp)
		describeConstraint (Unique names) = do
			writeStringCode "UNIQUE ("
			intercalateBuilder (writeStringCode ", ") $
				map (writeIdentifier . nameBase) names
			writeStringCode ")"

		describeConstraint (Check code) = do
			writeStringCode "CHECK ("
			writeStringCode code
			writeStringCode ")"

-- | Generate the query which insert a row.
makeInsertQuery :: TableDec -> Q Exp
makeInsertQuery (TableDec table _ fields _) =
	runQueryBuilder $ do
		writeCode "INSERT INTO " :: QueryBuilder String (Q Exp)
		writeIdentifier (show table)
		writeCode " ("
		intercalateBuilder (writeCode ", ") $
			map (\ (TableField name _ _) -> writeIdentifier name) fields
		writeCode ") VALUES ("
		intercalateBuilder (writeCode ", ") $
			map (\ (TableField _ _ boundName) -> writeParam [e| pack $(varE boundName) |]) fields
		writeCode ") RETURNING "
		writeIdentifier "$id"

-- | Generate the query which inserts multiple rows at once.
makeInsertManyQuery :: Name -> TableDec -> Q Exp
makeInsertManyQuery rows dec@(TableDec _ _ fields destructPat) =
	[e|
		let
			writeTuple $(pure destructPat) = do
				writeStringCode "(" :: QueryBuilder B.ByteString Value
				intercalateBuilder (writeStringCode ",") $(ListE <$> mapM packColumn fields)
				writeStringCode ")"
		in runQueryBuilder $ do
			writeStringCode $(stringE (tableInsertStatementBegin dec))
			intercalateBuilder (writeStringCode ",") (map writeTuple $(varE rows))
			writeStringCode $(stringE tableInsertStatementEnd)
	|]
	where
		packColumn (TableField _ _ boundName) = [e| writeColumn $(varE boundName) |]

-- | Generate the query for finding a row.
makeFindQuery :: Name -> TableDec -> Q Exp
makeFindQuery ref (TableDec table _ fields _) =
	runQueryBuilder $ do
		writeCode "SELECT " :: QueryBuilder String (Q Exp)
		intercalateBuilder (writeCode ", ") $
			map (\ (TableField name _ _) -> writeIdentifier name) fields
		writeCode " FROM "
		writeIdentifier (show table)
		writeCode " WHERE "
		writeIdentifier "$id"
		writeCode " = "
		writeParam [e| pack $(varE ref) |]

-- | Generate the query for updating a row.
makeUpdateQuery :: Name -> TableDec -> Q Exp
makeUpdateQuery ref (TableDec table _ fields _) =
	runQueryBuilder $ do
		writeCode "UPDATE " :: QueryBuilder String (Q Exp)
		writeIdentifier (show table)
		writeCode " SET "
		intercalateBuilder (writeCode ", ") $
			flip map fields $ \ (TableField name _ boundName) -> do
				writeIdentifier name
				writeCode " = "
				writeParam [e| pack $(varE boundName) |]
		writeCode " WHERE "
		writeIdentifier "$id"
		writeCode " = "
		writeParam [e| pack $(varE ref) |]

-- | Generate the query for deleting a row.
makeDeleteQuery :: Name -> Name -> Q Exp
makeDeleteQuery ref table =
	runQueryBuilder $ do
		writeCode "DELETE FROM " :: QueryBuilder String (Q Exp)
		writeIdentifier (show table)
		writeCode " WHERE "
		writeIdentifier "$id"
		writeCode " = "
		writeParam [e| pack $(varE ref) |]

-- | Implement relevant instances for the given table type.
implementClasses :: TableDec -> [TableConstraint] -> Q [Dec]
implementClasses dec@(TableDec table _ fields destructPat) constraints =
	[d|
		instance QueryTable $(conT table) where
			tableName _      = $(stringE (show table))
			tableIDName _    = "$id"
			tableSelectors _ = $(makeQuerySelectors fields)

		instance Result $(conT table) where
			queryResultProcessor = $(makeResultProcessor dec)

		instance Table $(conT table) where
			insert $(pure destructPat) = do
				rs <- query $(makeInsertQuery dec)
				case rs of
					(ref : _) -> pure ref
					_         -> throwError EmptyResult

			insertMany [] = pure []
			insertMany rows =
				query $(makeInsertManyQuery 'rows dec)

			find ref = do
				rs <- query $(makeFindQuery 'ref dec)
				case rs of
					(row : _) -> pure row
					_         -> throwError EmptyResult

			update ref $(pure destructPat) =
				query_ $(makeUpdateQuery 'ref dec)

			delete ref =
				query_ $(makeDeleteQuery 'ref table)

			createTableQuery _ =
				$(makeCreateQuery dec constraints)
	|]

-- | Check that each field's type has an implementation of 'Column'.
checkRecordFields :: [VarBangType] -> Q [TableField]
checkRecordFields fields =
	forM fields $ \ (name, _, typ) -> do
		ii <- isInstance ''Column [typ]
		unless ii $
			fail ("Type of field '" ++ show name ++ "' ('" ++ show typ ++
			      "') type does not implement '" ++ show ''Column ++ "'")

		TableField (nameBase name) typ <$> newName (nameBase name)

-- | Check that each constructor parameter type implements 'Column'.
checkNormalFields :: [BangType] -> Q [TableField]
checkNormalFields fields = do
	forM (fields `zip` [1 .. length fields]) $ \ ((_, typ), idx) -> do
		ii <- isInstance ''Column [typ]
		unless ii $
			fail ("Type of constructor parameter #" ++ show idx ++ " ('" ++ show typ ++
			      "') type does not implement '" ++ show ''Column ++ "'")

		TableField ("column" ++ show idx) typ <$> newName ("column" ++ show idx)

-- |
makeCtorPattern :: Name -> [TableField] -> Pat
makeCtorPattern ctor fields =
	ConP ctor (map (\ (TableField _ _ name) -> VarP name) fields)

-- | Verify that the given constructor is viable and construct a 'TableDec'.
checkTableCtor :: Name -> Con -> Q TableDec
checkTableCtor table (RecC ctor ctorFields) = do
	when (length ctorFields < 1)
	     (fail ("'" ++ show ctor ++ "' must have at least one field"))

	fields <- checkRecordFields ctorFields
	pure (TableDec table ctor fields (makeCtorPattern ctor fields))

checkTableCtor table (NormalC ctor ctorFields) = do
	when (length ctorFields < 1)
	     (fail ("'" ++ show ctor ++ "' must have at least one field"))

	fields <- checkNormalFields ctorFields
	pure (TableDec table ctor fields (makeCtorPattern ctor fields))

checkTableCtor table _ =
	fail ("'" ++ show table ++ "' must have a normal or record constructor")

-- | Make sure the given declaration can be used, then construct a 'TableDec'.
checkTableDec :: Name -> Dec -> Q TableDec
checkTableDec _ (DataD ctx tableName typeVars kind ctorNames _) = do
	when (length ctx > 0)
	     (fail ("'" ++ show tableName ++ "' must not have a context"))

	when (length typeVars > 0)
	     (fail ("'" ++ show tableName ++ "' must not use type variables"))

	when (length ctorNames /= 1)
	     (fail ("'" ++ show tableName ++ "' must have 1 constructor"))

	when (kind /= Nothing && kind /= Just StarT)
	     (fail ("'" ++ show tableName ++ "' must have kind *"))

	let [ctorName] = ctorNames

	checkTableCtor tableName ctorName

checkTableDec tableName _ =
	fail ("'" ++ show tableName ++ "' must declare a data type")

-- | Options to 'mkTable'.
data TableConstraint
	= Unique [Name]
	  -- ^ A combination of fields must be unique.
	  --   @Unique ['name1, 'name2, ...]@ works analogous to the table constraint
	  --   @UNIQUE (name1, name2, ...)@ in SQL.
	| Check String
	  -- ^ The given statement must evaluate to true. Just like @CHECK (statement)@ in SQL.
	deriving (Show, Eq, Ord)

-- | Implement the type classes 'QueryTable', 'Table' and 'Result' for the given type.
--
-- The given type must fulfill these requirements:
--
--   * Data type
--   * No type context
--   * No type variables
--   * One record constructor with 1 or more fields
--   * All field types must have an instance of 'Column'
--
-- Example:
--
-- @
-- {-\# LANGUAGE TemplateHaskell, QuasiQuotes \#-}
-- module Movies where
--
-- ...
--
-- data Movie = Movie {
--     movieTitle :: 'String',
--     movieYear  :: 'Int'
-- } deriving 'Show'
--
-- 'mkTable' ''Movie []
--
-- data Actor = Actor {
--     actorName :: 'String',
--     actorAge  :: 'Int'
-- } deriving 'Show'
--
-- 'mkTable' ''Actor ['Unique' ['actorName], 'Check' ['pgss'| actorAge >= 18 |]]
--
-- data MovieCast = MovieCast {
--     movieCastMovie :: 'Reference' Movie,
--     movieCastActor :: 'Reference' Actor
-- } deriving 'Show'
--
-- 'mkTable' ''MovieCast ['Unique' ['movieCastMovie, 'movieCastActor]]
-- @
--
-- In this example, 'Reference' takes care of adding the @FOREIGN KEY@ constraint, so we don't have
-- to.
--
mkTable :: Name -> [TableConstraint] -> Q [Dec]
mkTable name constraints = do
	info <- reify name
	case info of
		TyConI dec -> do
			tableDec <- checkTableDec name dec
			implementClasses tableDec constraints

		_ -> fail ("'" ++ show name ++ "' is not a type constructor")

-- | Check if the given name refers to a type.
isType :: Name -> Q Bool
isType name = do
	info <- reify name
	pure $ case info of
		TyConI _ -> True
		_        -> False

-- | Generate a 'Query' expression which will create the table described by the given type.
--
-- Example:
--
-- @
-- data Table = Table { myField :: Int }
-- 'mkTable' ''Table []
-- ...
-- 'query_' $('mkCreateQuery' ''Table)
-- @
--
mkCreateQuery :: Name -> Q Exp
mkCreateQuery name = do
	-- Is the given name a type?
	it <- isType name
	unless it (fail "Given name does not refer to a type.")

	-- Actual splice
	[e| createTableQuery (Proxy :: Proxy $(pure (ConT name))) |]
