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

import Data.Int
import Data.List
import Data.String
import Data.Typeable

import Language.Haskell.TH

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
	pack ref =
		pack (referenceID ref)

	unpack val =
		Reference <$> unpack val

	columnTypeName proxy =
		"BIGINT REFERENCES " ++ show (tableName tableProxy) ++
		" (" ++ show (tableIDName tableProxy) ++ ")"
		where
			tableProxy = (const Proxy :: Proxy (Reference a) -> Proxy a) proxy

	columnAllowNull _ = False

instance Result (Reference a) where
	queryResultProcessor =
		Reference <$> unpackColumn

-- | Qualify @a@ as a table type. 'mkTable' can implement this class for you.
class Table a where
	-- | Insert a row into the table and return a 'Reference' to the inserted row.
	insert :: a -> Errand (Reference a)

	-- | Find the row identified by the given reference.
	find :: Reference a -> Errand a

	-- | Update an existing row.
	update :: Reference a -> a -> Errand ()

	-- | Delete a row from the table.
	delete :: Reference a -> Errand ()

	-- | Generate the query which creates this table inside the database.
	-- Use 'mkCreateQuery' for convenience.
	createTableQuery :: Proxy a -> Query

-- | Generate the insert statement for a table.
insertStatementE :: Name -> [Name] -> Q Exp
insertStatementE table fields =
	[e| fromString ("INSERT INTO " ++ $(tableNameE table) ++ " (" ++
	                $(stringE columns) ++
	                ") VALUES (" ++
	                $(stringE values) ++
	                ") RETURNING " ++ $(tableIDNameE table)) |]
	where
		-- Column names
		columns =
			intercalate ", " (map (show . nameBase) fields)

		-- Value placeholders
		values =
			intercalate ", " (map (\ idx -> "$" ++ show idx) [1 .. length fields])

-- | Generate insert query for a table.
insertQueryE :: Name -> Name -> [Name] -> Q Exp
insertQueryE row table fields =
	[e| Query $(insertStatementE table fields) $(packParamsE row fields) |]

-- | Generate the find query for a table.
findStatementE :: Name -> Q Exp
findStatementE table =
	[e| fromString ("SELECT " ++ $(makeTableSelectorsE table) ++
	                " FROM " ++ $(tableNameE table) ++
	                " WHERE " ++ $(tableIDNameE table) ++ " = $1 LIMIT 1") |]

-- | Generate the find query for a table.
findQueryE :: Name -> Name -> Q Exp
findQueryE ref table =
	[e| Query $(findStatementE table) [pack (referenceID $(varE ref))] |]

-- | Generate the update statement for a table.
updateStatementE :: Name -> [Name] -> Q Exp
updateStatementE table fields =
	[e| fromString ("UPDATE " ++ $(tableNameE table) ++
	                " SET " ++ $(stringE statements) ++
	                " WHERE " ++ $(tableIDNameE table) ++ " = $1") |]
	where
		-- Produce a SET statement in form of 'name = $idx'
		makeStatement name idx =
			show (nameBase name) ++ " = $" ++ show idx

		-- Combined SET statements
		statements =
			intercalate "," (zipWith makeStatement fields [2 .. length fields + 1])

-- | Generate the update query for a table.
updateQueryE :: Name -> Name -> Name -> [Name] -> Q Exp
updateQueryE ref row table fields =
	[e| Query $(updateStatementE table fields)
	          (pack (referenceID $(varE ref)) : $(packParamsE row fields)) |]

-- | Generate the delete statement for a table.
deleteStatementE :: Name -> Q Exp
deleteStatementE table =
	[e| fromString ("DELETE FROM " ++ $(tableNameE table) ++
	                " WHERE " ++ $(tableIDNameE table) ++ " = $1") |]

-- | Generate the delete query for a table.
deleteQueryE :: Name -> Name -> Q Exp
deleteQueryE ref table =
	[e| Query $(deleteStatementE table) [pack (referenceID $(varE ref))] |]

-- | Generate the create statement for a table.
createStatementE :: Name -> [(Name, Type)] -> [TableConstraint] -> Q Exp
createStatementE table fields constraints =
	[e| fromString ("CREATE TABLE IF NOT EXISTS " ++ $(tableNameE table) ++ " (" ++
	                intercalate ", " ($idField : $fieldList ++ $constraintList) ++
	                ")") |]
	where
		-- ID column description
		idField =
			[e| $(tableIDNameE table) ++ " BIGSERIAL NOT NULL PRIMARY KEY" |]

		-- List of all column descriptions
		fieldList =
			ListE <$> mapM describeField fields

		-- Fetch column description for a field and its associated type
		describeField (name, typ) =
			[e| $(stringE (show (nameBase name))) ++ " " ++
			    makeColumnDescription (Proxy :: Proxy $(pure typ)) |]

		-- List of constraints
		constraintList =
			ListE <$> mapM describeConstraint constraints

		-- Generate the SQL described in the given constraint
		describeConstraint cont =
			case cont of
				Unique names ->
					stringE ("UNIQUE (" ++ intercalate ", " (map (show . nameBase) names) ++ ")")

				Check statement ->
					stringE ("CHECK (" ++ statement ++ ")")

-- | Generate the create query for a table.
createQueryE :: Name -> [(Name, Type)] -> [TableConstraint] -> Q Exp
createQueryE table fields constraints =
	[e| Query $(createStatementE table fields constraints) [] |]

-- | Generate an expression which gathers all records from a type and packs them into a list.
-- `packParamsE 'row ['field1, 'field2]` generates `[pack (field1 row), pack (field2 row)]`
packParamsE :: Name -> [Name] -> Q Exp
packParamsE row fields =
	ListE <$> mapM extract fields
	where
		extract name =
			[e| pack ($(varE name) $(varE row)) |]

-- | Generate the list of selectors.
tableSelectorsE :: [Name] -> Q Exp
tableSelectorsE fieldNames =
	ListE <$> mapM makeSelector fieldNames
	where
		makeSelector name =
			[e| SelectorField $(stringE (nameBase name)) |]

-- | Generate the result processor for a table.
tableResultProcessorE :: Name -> [Name] -> Q Exp
tableResultProcessorE tableCtor fieldNames = do
	bindingNames <- mapM (newName . nameBase) fieldNames
	pure (DoE (map makeBinding bindingNames ++
	           [makeConstruction bindingNames]))
	where
		-- Bind expression 'name <- unpackColumn'
		makeBinding name =
			BindS (VarP name) (VarE 'unpackColumn)

		-- Last expression 'pure (tableCtor boundNames...)'
		makeConstruction names =
			NoBindS (AppE (VarE 'pure)
			              (RecConE tableCtor
			                       (zip fieldNames (map VarE names))))

-- | Implement relevant instances for the given table type.
implementTableD :: Name -> Name -> [(Name, Type)] -> [TableConstraint] -> Q [Dec]
implementTableD table ctor fieldDecls constraints =
	[d|
		instance QueryTable $(conT table) where
			tableName _      = $(stringE (show table))
			tableIDName _    = "$id"
			tableSelectors _ = $(tableSelectorsE fields)

		instance Result $(conT table) where
			queryResultProcessor = $(tableResultProcessorE ctor fields)

		instance Table $(conT table) where
			insert row = do
				rs <- query $(insertQueryE 'row table fields)

				case rs of
					(ref : _) -> pure ref
					_         -> raiseErrandError UnexpectedEmptyResult

			find ref = do
				rs <- query $(findQueryE 'ref table)

				case rs of
					(row : _) -> pure row
					_         -> raiseErrandError UnexpectedEmptyResult

			update ref row =
				query_ $(updateQueryE 'ref 'row table fields)

			delete ref =
				query_ $(deleteQueryE 'ref table)

			createTableQuery _ =
				$(createQueryE table fieldDecls constraints)
	|]
	where
		fields = map fst fieldDecls

-- | Check that all field types have an instance of Column.
validateFields :: [(Name, Type)] -> Q ()
validateFields fields =
	forM_ fields $ \ (name, typ) -> do
		ii <- isInstance ''Column [typ]
		unless ii $
			fail ("\ESC[35m" ++ show name ++ "\ESC[0m's type does not have an instance of \ESC[34mColumn\ESC[0m")

-- | Options to 'mkTable'.
data TableConstraint
	= Unique [Name]
	  -- ^ A combination of fields must be unique.
	  --   @Unique ['name1, 'name2, ...]@ works analogous to the table constraint
	  --   @UNIQUE (name1, name2, ...)@ in SQL.
	| Check String
	  -- ^ The given statement must evaluate to true. Just like @CHECK (statement)@ in SQL.
	deriving (Show, Eq, Ord)

-- | Implement the relevant type classes for a data type to become a table type.
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
-- {-\# LANGUAGE TemplateHaskell \#-}
-- module Movies where
--
-- ...
--
-- data Movie = Movie {
--     movieTitle :: String,
--     movieYear  :: Int
-- } deriving Show
--
-- 'mkTable' ''Movie []
--
-- data Actor = Actor {
--     actorName :: String,
--     actorAge  :: Int
-- } deriving Show
--
-- 'mkTable' ''Actor [Unique ['actorName]]
--
-- data MovieCast = MovieCast {
--     movieCastMovie :: 'Reference' Movie,
--     movieCastActor :: 'Reference' Actor
-- } deriving Show
--
-- 'mkTable' ''MovieCast [Unique ['movieCastMovie, 'movieCastActor]]
-- @
--
-- In this example, 'Reference' takes care of adding the @FOREIGN KEY@ constraint, so we don't have
-- to.
--
mkTable :: Name -> [TableConstraint] -> Q [Dec]
mkTable name constraints = do
	info <- reify name
	case info of
		TyConI dec ->
			case dec of
				DataD [] _ [] _ [RecC ctor records@(_ : _)] _ -> do
					let fields = map (\ (fn, _, ft) -> (fn, ft)) records
					validateFields fields
					implementTableD name ctor fields constraints

				DataD (_ : _) _ _ _ _ _ ->
					fail ("\ESC[34m" ++ show name ++ "\ESC[0m has a context")

				DataD _ _ (_ : _) _ _ _ ->
					fail ("\ESC[34m" ++ show name ++ "\ESC[0m has one or more type variables")

				DataD _ _ _ _ [] _ ->
					fail ("\ESC[34m" ++ show name ++ "\ESC[0m does not have a constructor")

				DataD _ _ _ _ (_ : _ : _) _ ->
					fail ("\ESC[34m" ++ show name ++ "\ESC[0m has more than one constructor")

				DataD _ _ _ _ [RecC _ []] _ ->
					fail ("\ESC[34m" ++ show name ++ "\ESC[0m has an empty record constructor")

				DataD _ _ _ _ [_] _ ->
					fail ("\ESC[34m" ++ show name ++ "\ESC[0m does not have a record constructor")

				_ -> fail ("\ESC[34m" ++ show name ++ "\ESC[0m is not an eligible data type")

		_ -> fail ("\ESC[34m" ++ show name ++ "\ESC[0m is not a type constructor")

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
