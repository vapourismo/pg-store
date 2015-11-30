{-# LANGUAGE TemplateHaskell, RecordWildCards #-}

module Database.PostgreSQL.Store.Table (
	TableDescription (..),
	Table (..),
	Row (..),
	Reference (..),
	HasID (..),
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

-- | Description of a table type
data TableDescription a = TableDescription {
	-- | Table name
	tableName :: String,

	-- | Identifier column name - 'pgsq' does not respect this value when generating row identifiers
	tableIdentColumn :: String
} deriving (Show, Eq, Ord)

-- | Resolved row
data Row a = Row {
	-- | Identifier
	rowID :: Int64,

	-- | Value
	rowValue :: a
} deriving (Show, Eq, Ord)

-- | Reference to a row
newtype Reference a = Reference Int64
	deriving (Show, Eq, Ord)

instance (Table a) => Column (Reference a) where
	pack ref = pack (referenceID ref)
	unpack val = Reference <$> unpack val

	columnDescription =
		make tableDescription
		where
			make :: TableDescription a -> ColumnDescription (Reference a)
			make TableDescription {..} =
				ColumnDescription {
					columnTypeName = "bigint references " ++ tableName ++ " (" ++ tableIdentColumn ++ ")",
					columnTypeNull = False
				}

class Table a where
	-- | Insert a row into the table and return a 'Reference' to the inserted row.
	insert :: a -> Errand (Reference a)

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

	-- | Describe the table.
	tableDescription :: TableDescription a

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
			"INSERT INTO " ++ sanitizeName name ++ " (" ++
				intercalate ", " columns ++
			") VALUES (" ++
				intercalate ", " values ++
			") RETURNING " ++ identField name

		columns =
			map sanitizeName fields

		values =
			map (\ idx -> "$" ++ show idx) [1 .. length fields]

-- | Generate the update query for a table.
updateQueryE :: Name -> Int -> Q Exp
updateQueryE name numFields =
	[e| fromString $(stringE query) |]
	where
		query =
			"UPDATE " ++ sanitizeName name ++
			" SET " ++ intercalate ", " values ++
			" WHERE " ++ identField name ++ " = $1"

		values =
			map (\ idx -> "$" ++ show idx) [2 .. numFields + 1]

-- | Generate the delete query for a table.
deleteQueryE :: Name -> Q Exp
deleteQueryE name =
	[e| fromString $(stringE query) |]
	where
		query = "DELETE FROM " ++ sanitizeName name ++ " WHERE " ++ identField name ++ " = $1"

-- | Generate the create query for a table.
createQueryE :: Name -> [(Name, Type)] -> Q Exp
createQueryE name fields =
	[e| fromString ($(stringE queryBegin) ++
	                intercalate ", " ($(stringE anchorDescription) : $(descriptions)) ++
	                $(stringE queryEnd)) |]
	where
		queryBegin = "CREATE TABLE IF NOT EXISTS " ++ sanitizeName name ++ " ("
		queryEnd = ")"

		anchorDescription =
			identField name ++ " BIGSERIAL NOT NULL PRIMARY KEY"

		descriptions =
			ListE <$> mapM describeField fields

		describeField (fname, ftype) =
			[e| $(stringE (sanitizeName fname)) ++ " " ++
			    makeColumnDescription (columnDescription :: ColumnDescription $(pure ftype)) |]

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
	[e| columnInfo (fromString $(stringE (sanitizeName name))) |]

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
	[e| do idNfo <- columnInfo (fromString $(stringE (identField table)))
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
	[e| do idNfo <- columnInfo (fromString $(stringE (identField table)))
	       foreachRow (\ row -> Reference <$> unpackCellValue row idNfo) |]

-- | Generate an expression which instantiates a description for a given table.
tableDescriptionE :: Name -> Q Exp
tableDescriptionE table =
	[e| TableDescription {
	        tableName = $(stringE (sanitizeName table)),
	        tableIdentColumn = $(stringE (identField table))
	    } |]

-- | Implement an instance 'Table' for the given type.
implementTableD :: Name -> Name -> [(Name, Type)] -> Q [Dec]
implementTableD table ctor fields = do
	[d| instance Table $(pure (ConT table)) where
	        insert row = do
	            rs <- query Query {
	                queryStatement = $(insertQueryE table fieldNames),
	                queryParams    = $(packParamsE 'row fieldNames)
	            }

	            case rs of
	            	(ref : _) -> pure ref
	            	_         -> raiseErrandError "Result set for insertion is empty"

	        update ref row =
	            query_ Query {
	                queryStatement = $(updateQueryE table (length fieldNames)),
	                queryParams    = pack (referenceID ref) : $(packParamsE 'row fieldNames)
	            }

	        delete ref =
	            query_ Query {
	                queryStatement = $(deleteQueryE table),
	                queryParams    = [pack (referenceID ref)]
	            }

	        createQuery _ =
	            Query {
	                queryStatement = $(createQueryE table fields),
	                queryParams    = []
	            }

	        tableResultProcessor =
	            $(tableResultProcessorE table ctor fieldNames)

	        tableRefResultProcessor =
	        	$(tableRefResultProcessorE table)

	        tableDescription =
	            $(tableDescriptionE table) |]
	where
		fieldNames = map fst fields

-- | Check that all field types have an instance of Column.
validateFields :: [(Name, Type)] -> Q ()
validateFields fields =
	forM_ fields $ \ (name, typ) -> do
		ii <- isInstance ''Column [typ]
		unless ii $
			fail ("\ESC[35m" ++ show name ++ "\ESC[0m's type does not have an instance of \ESC[34mColumn\ESC[0m")

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
-- module Movies where
--
-- ...
--
-- data Movie = Movie {
--     movieTitle :: String,
--     movieYear  :: Int
-- } deriving Show
--
-- 'mkTable' ''Movie
--
-- data Actor = Actor {
--     actorName :: String,
--     actorAge  :: Int
-- } deriving Show
--
-- 'mkTable' ''Actor
--
-- data MovieCast = MovieCast {
--     movieCastMovie :: 'Reference' Movie,
--     movieCastActor :: 'Reference' Actor
-- } deriving Show
--
-- 'mkTable' ''MovieCast
-- @
--
mkTable :: Name -> Q [Dec]
mkTable name = do
	info <- reify name
	case info of
		TyConI dec ->
			case dec of
				DataD [] _ [] [RecC ctor records@(_ : _)] _ -> do
					let fields = map (\ (fn, _, ft) -> (fn, ft)) records
					validateFields fields
					implementTableD name ctor fields

				DataD (_ : _) _ _ _ _ ->
					fail ("\ESC[34m" ++ show name ++ "\ESC[0m has a context")

				DataD _ _ (_ : _) _ _ ->
					fail ("\ESC[34m" ++ show name ++ "\ESC[0m has one or more type variables")

				DataD _ _ _ [] _ ->
					fail ("\ESC[34m" ++ show name ++ "\ESC[0m does not have a constructor")

				DataD _ _ _ (_ : _ : _) _ ->
					fail ("\ESC[34m" ++ show name ++ "\ESC[0m has more than one constructor")

				DataD _ _ _ [RecC _ []] _ ->
					fail ("\ESC[34m" ++ show name ++ "\ESC[0m has an empty record constructor")

				DataD _ _ _ [_] _ ->
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
-- 'mkTable' ''Table
-- ...
-- 'query_' $('mkCreateQuery' ''Table)
-- @
--
mkCreateQuery :: Name -> Q Exp
mkCreateQuery name = do
	-- Is the given name a type?
	it <- isType name
	unless it $
		fail ("Given name does not refer to a type.")

	-- Actual splice
	[e| createQuery (Proxy :: Proxy $(pure (ConT name))) |]