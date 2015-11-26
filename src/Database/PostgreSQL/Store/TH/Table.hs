{-# LANGUAGE TemplateHaskell #-}

module Database.PostgreSQL.Store.TH.Table where

import           Control.Monad.Trans.Class

import           Data.List
import           Data.String
import           Data.Typeable

import           Database.PostgreSQL.Store.Types
import           Database.PostgreSQL.Store.Monad

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

-- | Generate the empty update query for a table.
emptyUpdateQueryE :: Name -> Q Exp
emptyUpdateQueryE name =
	[e| fromString $(stringE query) |]
	where
		query =
			"UPDATE " ++ sanitizeName name ++
			" SET " ++ identField name ++ " = $1" ++
			" WHERE " ++ identField name ++ " = $1"

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
			    show (columnDescription :: ColumnDescription $(pure ftype)) |]

-- | Generate the drop query for a table.
dropQueryE :: Name -> Q Exp
dropQueryE name =
	[e| fromString $(stringE query) |]
	where
		query = "DROP TABLE IF EXISTS " ++ sanitizeName name

-- | Generate an expression which gathers all records from a type and packs them into a list.
-- `packParamsE 'row ['field1, 'field2]` generates `[pack (field1 row), pack (field2 row)]`
packParamsE :: Name -> [Name] -> Q Exp
packParamsE row fields =
	ListE <$> mapM extract fields
	where
		extract name =
			[e| pack ($(varE name) $(varE row)) |]

-- | Generate an expression which fetches the column number for a column name.
columnNumberE :: Name -> Q Exp
columnNumberE name =
	[e| columnNumber (fromString $(stringE (sanitizeName name))) |]

-- | Generate an expression which fetches the column number for the identifying column.
idColumnNumberE :: Name -> Q Exp
idColumnNumberE table =
	[e| columnNumber (fromString $(stringE (identField table))) |]

-- | Generate an expression which retrieves the OID for a column number.
columnTypeE :: Name -> Q Exp
columnTypeE col =
	[e| columnType $(varE col) |]

-- | Generate an expression which retrieves the format for a column number.
columnFormatE :: Name -> Q Exp
columnFormatE col =
	[e| columnFormat $(varE col) |]

-- | Generate an expression which gathers information about a column.
columnInfoE :: Name -> Q Exp
columnInfoE name =
	[e| columnInfo (fromString $(stringE (sanitizeName name))) |]

-- | Generate an expression which gathers information about the identifying column.
identColumnInfo :: Name -> Q Exp
identColumnInfo table =
	[e| columnInfo (fromString $(stringE (identField table))) |]

-- | Generate a query which binds information about a column to the column's info name.
bindColumnInfoS :: Name -> Q Stmt
bindColumnInfoS name =
	BindS (VarP (columnInfoName name)) <$> columnInfoE name

-- | Generate a name which is reserved for information about a column.
columnInfoName :: Name -> Name
columnInfoName name =
	mkName (nameBase name ++ "_info")

-- | Generate an expression which fetches data for a cell.
columnDataE :: Name -> Name -> Q Exp
columnDataE row col =
	[e| cellData $(varE row) $(varE col) |]

-- | Generate an expression which unpacks a column at a given row.
unpackColumnE :: Name -> Name -> Q Exp
unpackColumnE row name =
	[e| unpackCellValue $(varE row) $(varE (columnInfoName name)) |]

-- | Generate an expression which unpacks the identifying column at a given
unpackIdentColumnE :: Name -> Name -> Q Exp
unpackIdentColumnE row idNfo =
	[e| unpackCellValue $(varE row) $(varE idNfo) |]

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
unpackInstanceE :: Name -> Name -> [Name] -> Q Exp
unpackInstanceE ctor row fields = do
	boundFields <- mapM (bindColumnS row) fields
	unboundConstruction <- constructRecordE ctor fields
	pure (DoE (boundFields ++ [NoBindS unboundConstruction]))

-- | Generate an expression which traverses all rows in order to unpack table instances from them.
unpackRowsE :: Name -> Name -> [Name] -> Q Exp
unpackRowsE table ctor fields =
	[e| do idNfo <- $(identColumnInfo table)
	       foreachRow $ \ row ->
	           Row <$> $(unpackIdentColumnE 'row 'idNfo)
	               <*> $(unpackInstanceE ctor 'row fields) |]

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
implementTableD table ctor fields =
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

	        dropQuery _ =
	            Query {
	                queryStatement = $(dropQueryE table),
	                queryParams    = []
	            }

	        tableResultProcessor =
	            $(fromResultE table ctor fieldNames)

	        tableDescription =
	            $(tableDescriptionE table) |]
	where
		fieldNames = map fst fields

-- | Make a type ready to be used as a table.
mkTable :: Name -> Q [Dec]
mkTable name = do
	info <- reify name
	case info of
		TyConI (DataD [] _ [] [RecC ctor records] _) | length records > 0 ->
			implementTableD name ctor fields
			where
				fields = map (\ (fn, _, ft) -> (fn, ft)) records

		_ -> fail "Need type-constructor for a context-less type-variable-free data type with only one record constructor and 1 or more records"

-- | Inline the create table query of a table.
mkCreateQuery :: Name -> Q Exp
mkCreateQuery name =
	[e| createQuery (Proxy :: Proxy $(pure (ConT name))) |]

-- | Inline the drop table query of a table.
mkDropQuery :: Name -> Q Exp
mkDropQuery name =
	[e| dropQuery (Proxy :: Proxy $(pure (ConT name))) |]
