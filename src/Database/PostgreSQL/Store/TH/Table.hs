{-# LANGUAGE TemplateHaskell #-}

module Database.PostgreSQL.Store.TH.Table where

import           Control.Monad
import           Control.Monad.Trans.Class

import           Data.List
import           Data.String
import           Data.Typeable

import           Database.PostgreSQL.Store.Internal

import           Language.Haskell.TH

-- | Unqualify a name.
unqualifyName :: Name -> Name
unqualifyName = mkName . nameBase

-- | Generate the sanitized representation of a name.
sanitizeName :: Name -> String
sanitizeName name =
	"\"" ++ show name ++ "\""

-- | Generate the name of the identifying field.
identField :: Name -> String
identField name =
	"\"" ++ maybe [] (++ ".") (nameModule name) ++ "$id\""

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
fromResultE :: Name -> Name -> [Name] -> Q Exp
fromResultE table ctor fields = do
	infoBinds <- mapM bindColumnInfoS fields
	rowTraversal <- unpackRowsE table ctor fields
	pure (DoE (infoBinds ++ [NoBindS rowTraversal]))

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
	        insertQuery row =
	            Query {
	                queryStatement = $(insertQueryE table fieldNames),
	                queryParams    = $(packParamsE 'row fieldNames)
	            }

	        updateQuery (Row rid row) =
	            Query {
	                queryStatement = $(updateQueryE table (length fieldNames)),
	                queryParams    = pack rid : $(packParamsE 'row fieldNames)
	            }

	        deleteQuery ref =
	            Query {
	                queryStatement = $(deleteQueryE table),
	                queryParams    = [pack (referenceID ref)]
	            }

	        createQuery _ =
	            Query {
	                queryStatement = $(createQueryE table fields),
	                queryParams    = []
	            }

	        tableResultProcessor =
	            $(fromResultE table ctor fieldNames)

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

-- | Make a type ready to be used as a table.
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

-- | Inline the create table query of a table.
mkCreateQuery :: Name -> Q Exp
mkCreateQuery name = do
	-- Is the given name a type?
	it <- isType name
	unless it $
		fail ("Given name does not refer to a type.")

	-- Make sure the given name refers to a type which implements Table
	ii <- isInstance ''Table [ConT name]
	unless ii $
		fail ("Type \ESC[34m" ++ show name ++ "\ESC[0m has to be an instance of \ESC[34mTable\ESC[0m.\n\
		      \    To fix this add the following after the declaration of \ESC[34m" ++ show name ++ "\ESC[0m:\n\n\
		      \        mkTable ''\ESC[34m" ++ show name ++ "\ESC[0m\n")

	-- Actual splice
	[e| createQuery (Proxy :: Proxy $(pure (ConT name))) |]
