{-# LANGUAGE TemplateHaskell, RecordWildCards, BangPatterns #-}

-- |
-- Module:     Database.PostgreSQL.Store.Table
-- Copyright:  (c) Ole Krüger 2015-2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.Table (
	TableDescription (..),
	Table (..),
	Row (..),
	Reference (..),
	HasID (..),
	TableConstraint (..),
	mkTable,
	mkCreateQuery
) where

import Control.Monad
import Control.Monad.Trans.Class

import Data.Int
import Data.List
import Data.String
import Data.Typeable

import Language.Haskell.TH

import Database.PostgreSQL.Store.Query
import Database.PostgreSQL.Store.Columns
import Database.PostgreSQL.Store.Result
import Database.PostgreSQL.Store.Errand

-- | Resolved row
data Row a = Row {
	-- | Identifier
	rowID :: !Int64,

	-- | Value
	rowValue :: !a
} deriving (Show, Eq, Ord)

-- | Reference to a row
newtype Reference a = Reference Int64
	deriving (Show, Eq, Ord)

instance (Table a) => Column (Reference a) where
	pack ref =
		pack (referenceID ref)

	unpack val =
		Reference <$> unpack val

	describeColumn proxy =
		ColumnDescription {
			columnTypeName = "bigint references \"" ++ tableName ++ "\" (\"" ++ tableIdentifier ++ "\")",
			columnTypeNull = False
		}
		where
			coerceProxy :: Proxy (Reference a) -> Proxy a
			coerceProxy _ = Proxy

			TableDescription {..} =
				describeTable (coerceProxy proxy)

class (DescribableTable a) => Table a where
	-- | Insert a row into the table and return a 'Reference' to the inserted row.
	insert :: a -> Errand (Reference a)

	-- | Find the row identified by the given reference.
	find :: (HasID i) => i a -> Errand (Row a)

	-- | Update an existing row.
	update :: (HasID i) => i a -> a -> Errand ()

	-- | Delete a row from the table.
	delete :: (HasID i) => i a -> Errand ()

	-- | Generate the query which creates this table inside the database.
	-- Use @mkCreateQuery@ for convenience.
	createQuery :: Proxy a -> Query

	-- | Extract rows from a result set.
	tableResultProcessor :: ResultProcessor [Row a]

	-- | Extract only a 'Reference' to each row.
	tableRefResultProcessor :: ResultProcessor [Reference a]

instance (Table a) => Result (Row a) where
	resultProcessor = tableResultProcessor

instance (Table a) => Result (Reference a) where
	resultProcessor = tableRefResultProcessor

-- | A value of that type contains an ID.
class HasID a where
	-- | Retrieve the underlying ID.
	referenceID :: a b -> Int64

instance HasID Row where
	referenceID (Row rid _) = rid

instance HasID Reference where
	referenceID (Reference rid) = rid

-- | Unqualify a name.
unqualifyName :: Name -> Name
unqualifyName = mkName . nameBase

-- | Generate the insert query for a table.
insertQueryE :: Name -> [Name] -> Q Exp
insertQueryE name fields =
	[e| fromString $(stringE query) |]
	where
		query =
			"INSERT INTO " ++ sanitizeName' name ++ " (" ++
				intercalate ", " columns ++
			") VALUES (" ++
				intercalate ", " values ++
			") RETURNING " ++ identField' name

		columns =
			map (\ nm -> "\"" ++ sanitizeName nm ++ "\"") fields

		values =
			map (\ idx -> "$" ++ show idx) [1 .. length fields]

-- | Generate the select query for a table row.
findQueryE :: Name -> Q Exp
findQueryE name =
	[e| fromString $(stringE query) |]
	where
		query =
			"SELECT * FROM " ++ sanitizeName' name ++
			" WHERE " ++ identField' name ++ " = $1 LIMIT 1"

-- | Generate the update query for a table.
updateQueryE :: Name -> [Name] -> Q Exp
updateQueryE name fields =
	[e| fromString $(stringE query) |]
	where
		query =
			"UPDATE " ++ sanitizeName' name ++
			" SET " ++ intercalate ", " values ++
			" WHERE " ++ identField' name ++ " = $1"

		values =
			map (\ (nm, idx) -> sanitizeName' nm ++ " = $" ++ show idx)
			    (zip fields [2 .. length fields + 1])

-- | Generate the delete query for a table.
deleteQueryE :: Name -> Q Exp
deleteQueryE name =
	[e| fromString $(stringE query) |]
	where
		query =
			"DELETE FROM " ++ sanitizeName' name ++
			" WHERE " ++ identField' name ++ " = $1"

-- | Generate the create query for a table.
createQueryE :: Name -> [(Name, Type)] -> [TableConstraint] -> Q Exp
createQueryE name fields constraints =
	[e| fromString ($(stringE queryBegin) ++
	                intercalate ", " ($(stringE anchorDescription) :
	                                  $fieldList ++
	                                  $constraintList) ++
	                $(stringE queryEnd)) |]
	where
		queryBegin = "CREATE TABLE IF NOT EXISTS " ++ sanitizeName' name ++ " ("
		queryEnd = ")"

		anchorDescription =
			identField' name ++ " BIGSERIAL NOT NULL PRIMARY KEY"

		fieldList =
			ListE <$> mapM describeField fields

		describeField (fname, ftype) =
			[e| $(stringE (sanitizeName' fname)) ++ " " ++
			    makeColumnDescription (describeColumn (Proxy :: Proxy $(pure ftype))) |]

		constraintList =
			ListE <$> mapM describeConstraint constraints

		describeConstraint cont =
			case cont of
				Unique names ->
					stringE ("UNIQUE (" ++ intercalate ", " (map sanitizeName' names) ++ ")")

				ForeignKey names table tableNames ->
					stringE ("FOREIGN KEY (" ++ intercalate ", " (map sanitizeName' names) ++
					         ") REFERENCES " ++ sanitizeName' table ++
					         "(" ++ intercalate ", " (map sanitizeName' tableNames) ++ ")")

				Check statement ->
					stringE ("CHECK (" ++ statement ++ ")")

-- | Generate an expression which gathers all records from a type and packs them into a list.
-- `packParamsE 'row ['field1, 'field2]` generates `[pack (field1 row), pack (field2 row)]`
packParamsE :: Name -> [Name] -> Q Exp
packParamsE row fields =
	ListE <$> mapM extract fields
	where
		extract name =
			[e| pack ($(varE name) $(varE row)) |]

-- | Generate an expression which gathers information about a column.
columnInfoE :: Name -> Q Exp
columnInfoE name =
	[e| columnInfo (fromString $(stringE (sanitizeName' name))) |]

-- | Generate a query which binds information about a column to the column's info name.
bindColumnInfoS :: Name -> Q Stmt
bindColumnInfoS name =
	BindS (VarP (columnInfoName name)) <$> columnInfoE name

-- | Generate a name which is reserved for information about a column.
columnInfoName :: Name -> Name
columnInfoName name =
	mkName (nameBase name ++ "_info")

-- | Generate an expression which unpacks a column at a given row.
unpackColumnE :: Name -> Name -> Q Exp
unpackColumnE row name =
	[e| unpackCellValue $(varE row) $(varE (columnInfoName name)) |]

-- | Generate a query which binds the unpacked data for a column at a given row to the column's name.
bindColumnS :: Name -> Name -> Q Stmt
bindColumnS row name =
	BindS (VarP (unqualifyName name)) <$> unpackColumnE row name

-- | Generate an expression which uses a record constructor with variables that correspond to its fields.
constructRecordE :: Name -> [Name] -> Q Exp
constructRecordE ctor fields =
	[e| lift (pure $(pure construction)) |]
	where
		construction = RecConE ctor (map (\ n -> (n, VarE (unqualifyName n))) fields)

-- | Generate an expression which unpacks a table instance from a given row.
unpackRowE :: Name -> Name -> [Name] -> Q Exp
unpackRowE ctor row fields = do
	boundFields <- mapM (bindColumnS row) fields
	unboundConstruction <- constructRecordE ctor fields
	pure (DoE (boundFields ++ [NoBindS unboundConstruction]))

-- | Generate an expression which traverses all rows in order to unpack table instances from them.
unpackRowsE :: Name -> Name -> [Name] -> Q Exp
unpackRowsE table ctor fields =
	[e| do idNfo <- columnInfo (fromString $(stringE (identField' table)))
	       foreachRow $ \ row ->
	           Row <$> unpackCellValue row idNfo
	               <*> $(unpackRowE ctor 'row fields) |]

-- | Generate an expression which retrieves a table instance from each row.
tableResultProcessorE :: Name -> Name -> [Name] -> Q Exp
tableResultProcessorE table ctor fields = do
	infoBinds <- mapM bindColumnInfoS fields
	rowTraversal <- unpackRowsE table ctor fields
	pure (DoE (infoBinds ++ [NoBindS rowTraversal]))

-- | Generate an expression which retrieves a reference to each row.
tableRefResultProcessorE :: Name -> Q Exp
tableRefResultProcessorE table =
	[e| do idNfo <- columnInfo (fromString $(stringE (identField' table)))
	       foreachRow (\ row -> Reference <$> unpackCellValue row idNfo) |]

-- | Implement an instance 'Table' for the given type.
implementTableD :: Name -> Name -> [(Name, Type)] -> [TableConstraint] -> Q [Dec]
implementTableD table ctor fields constraints =
	[d| instance DescribableTable $(conT table) where
	        describeTableName _ =
	            $(stringE (sanitizeName table))

	        describeTableIdentifier _ =
	            $(stringE (identField table))

	    instance Table $(conT table) where
	        insert row = do
	            rs <- query Query {
	                queryStatement = $(insertQueryE table fieldNames),
	                queryParams    = $(packParamsE 'row fieldNames)
	            }

	            case rs of
	                (ref : _) -> pure ref
	                _         -> raiseErrandError UnexpectedEmptyResult

	        find ref = do
	            rs <- query Query {
	                queryStatement = $(findQueryE table),
	                queryParams    = [pack (referenceID ref)]
	            }

	            case rs of
	                (row : _) -> pure row
	                _         -> raiseErrandError UnexpectedEmptyResult

	        update ref row =
	            query_ Query {
	                queryStatement = $(updateQueryE table fieldNames),
	                queryParams    = pack (referenceID ref) : $(packParamsE 'row fieldNames)
	            }

	        delete ref =
	            query_ Query {
	                queryStatement = $(deleteQueryE table),
	                queryParams    = [pack (referenceID ref)]
	            }

	        createQuery _ =
	            Query {
	                queryStatement = $(createQueryE table fields constraints),
	                queryParams    = []
	            }

	        tableResultProcessor =
	            $(tableResultProcessorE table ctor fieldNames)

	        tableRefResultProcessor =
	            $(tableRefResultProcessorE table) |]
	where
		fieldNames = map fst fields

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
	  --   @Unique ['name1, 'name2, ...]@ works analogous to the following table constraint:
	  --   @UNIQUE (name1, name2, ...)@
	| ForeignKey [Name] Name [Name]
	  -- ^ A combination of fields references another combination of fields from a different table.
	  --   @ForeignKey ['name1, 'name2, ...] ''RefTable ['refname1, 'refname2, ...]@ works like this
	  --   table constraint in SQL:
	  --   @FOREIGN KEY (name1, name2, ...) REFERENCES RefTable(refname1, refname2, ...)@
	| Check String
	  -- ^ The given statement must evaluate to true.
	deriving (Show, Eq, Ord)

-- | Implement 'Table' for a data type. The given type must fulfill these requirements:
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

-- | Generate a 'Query' which will create the table described my the given type.
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
	[e| createQuery (Proxy :: Proxy $(pure (ConT name))) |]
