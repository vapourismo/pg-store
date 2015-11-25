{-# LANGUAGE TemplateHaskell #-}

module Database.PostgreSQL.Store.TH (
	mkTable,
	mkCreateStatement,
	mkDropStatement
) where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Data.List
import           Data.String
import           Data.Typeable
import           Database.PostgreSQL.Store.Types
import qualified Database.PostgreSQL.LibPQ as P
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

-- | Generate the insert statement for a table.
insertStatementE :: Name -> [Name] -> Q Exp
insertStatementE name fields =
	[e| fromString $(stringE statement) |]
	where
		statement =
			"INSERT INTO " ++ sanitizeName name ++ " (" ++
				intercalate ", " columns ++
			") VALUES (" ++
				intercalate ", " values ++
			") RETURNING " ++ identField name

		columns =
			map sanitizeName fields

		values =
			map (\ idx -> "$" ++ show idx) [1 .. length fields]

-- | Generate the empty update statement for a table.
emptyUpdateStatementE :: Name -> Q Exp
emptyUpdateStatementE name =
	[e| fromString $(stringE statement) |]
	where
		statement =
			"UPDATE " ++ sanitizeName name ++
			" SET " ++ identField name ++ " = $1" ++
			" WHERE " ++ identField name ++ " = $1"

-- | Generate the update statement for a table.
updateStatementE :: Name -> Int -> Q Exp
updateStatementE name numFields =
	[e| fromString $(stringE statement) |]
	where
		statement =
			"UPDATE " ++ sanitizeName name ++
			" SET " ++ intercalate ", " values ++
			" WHERE " ++ identField name ++ " = $1"

		values =
			map (\ idx -> "$" ++ show idx) [2 .. numFields + 1]

-- | Generate the delete statement for a table.
deleteStatementE :: Name -> Q Exp
deleteStatementE name =
	[e| fromString $(stringE statement) |]
	where
		statement = "DELETE FROM " ++ sanitizeName name ++ " WHERE " ++ identField name ++ " = $1"

-- | Generate the create statement for a table.
createStatementE :: Name -> [(Name, Type)] -> Q Exp
createStatementE name fields =
	[e| fromString ($(stringE statementBegin) ++
	                intercalate ", " ($(stringE anchorDescription) : $(descriptions)) ++
	                $(stringE statementEnd)) |]
	where
		statementBegin = "CREATE TABLE IF NOT EXISTS " ++ sanitizeName name ++ " ("
		statementEnd = ")"

		anchorDescription =
			identField name ++ " BIGSERIAL NOT NULL PRIMARY KEY"

		descriptions =
			ListE <$> mapM describeField fields

		describeField (fname, ftype) =
			[e| $(stringE (sanitizeName fname)) ++ " " ++
			    show (columnTypeDescription :: ColumnTypeDescription $(pure ftype)) |]

-- | Generate the drop statement for a table.
dropStatementE :: Name -> Q Exp
dropStatementE name =
	[e| fromString $(stringE statement) |]
	where
		statement = "DROP TABLE IF EXISTS " ++ sanitizeName name

-- | Generate an expression which gathers all records from a type and packs them into a list.
-- `packParamsE 'row ['field1, 'field2]` generates `[pack (field1 row), pack (field2 row)]`
packParamsE :: Name -> [Name] -> Q Exp
packParamsE row fields =
	ListE <$> mapM extract fields
	where
		extract name =
			[e| pack ($(varE name) $(varE row)) |]

-- | Generate an expression which fetches the column number for a column name.
columnNumberE :: Name -> Name -> Q Exp
columnNumberE res name =
	[e| MaybeT (P.fnumber $(varE res) (fromString $(stringE (sanitizeName name)))) |]

-- | Generate an expression which fetches the column number for the identifying column.
idColumnNumberE :: Name -> Name -> Q Exp
idColumnNumberE res table =
	[e| MaybeT (P.fnumber $(varE res) (fromString $(stringE (identField table)))) |]

-- | Generate an expression which retrieves the OID for a column number.
columnTypeE :: Name -> Name -> Q Exp
columnTypeE res col =
	[e| lift (P.ftype $(varE res) $(varE col)) |]

-- | Generate an expression which retrieves the format for a column number.
columnFormatE :: Name -> Name -> Q Exp
columnFormatE res col =
	[e| lift (P.fformat $(varE res) $(varE col)) |]

-- | Generate an expression which gathers information about a column.
columnInfoE :: Name -> Name -> Q Exp
columnInfoE res name =
	[e| do col <- $(columnNumberE res name)
	       oid <- $(columnTypeE res 'col)
	       fmt <- $(columnFormatE res 'col)
	       pure (col, oid, fmt) |]

-- | Generate an expression which gathers information about the identifying column.
identColumnInfo :: Name -> Name -> Q Exp
identColumnInfo res table =
	[e| do col <- $(idColumnNumberE res table)
	       oid <- $(columnTypeE res 'col)
	       fmt <- $(columnFormatE res 'col)
	       pure (col, oid, fmt) |]

-- | Generate a statement which binds information about a column to the column's info name.
bindColumnInfoS :: Name -> Name -> Q Stmt
bindColumnInfoS res name =
	BindS (VarP (columnInfoName name)) <$> columnInfoE res name

-- | Generate a name which is reserved for information about a column.
columnInfoName :: Name -> Name
columnInfoName name =
	mkName (nameBase name ++ "_info")

-- | Generate an expression which fetches data for a cell.
columnDataE :: Name -> Name -> Name -> Q Exp
columnDataE res row col =
	[e| MaybeT (P.getvalue' $(varE res) $(varE row) $(varE col)) |]

-- | Generate an expression which unpacks a column at a given row.
unpackColumnE :: Name -> Name -> Name -> Q Exp
unpackColumnE res row name =
	[e| do let (col, oid, fmt) = $(varE (columnInfoName name))
	       dat <- $(columnDataE res row 'col)
	       let val = Value {
	           valueType = oid,
	           valueData = dat,
	           valueFormat = fmt
	       }
	       MaybeT (pure (unpack val)) |]

-- | Generate an expression which unpacks the identifying column at a given
unpackIdentColumnE :: Name -> Name -> Name -> Q Exp
unpackIdentColumnE res row idNfo =
	[e| do let (col, oid, fmt) = $(varE idNfo)
	       dat <- $(columnDataE res row 'col)
	       let val = Value {
	           valueType = oid,
	           valueData = dat,
	           valueFormat = fmt
	       }
	       MaybeT (pure (unpack val)) |]

-- | Generate a statement which binds the unpacked data for a column at a given row to the column's name.
bindColumnS :: Name -> Name -> Name -> Q Stmt
bindColumnS res row name =
	BindS (VarP (unqualifyName name)) <$> unpackColumnE res row name

-- | Generate an expression which uses a record constructor with variables that correspond to its fields.
constructRecordE :: Name -> [Name] -> Q Exp
constructRecordE ctor fields =
	[e| lift (pure $(pure construction)) |]
	where
		construction = RecConE ctor (map (\ n -> (n, VarE (unqualifyName n))) fields)

-- | Generate an expression which unpacks a table instance from a given row.
unpackInstanceE :: Name -> Name -> Name -> [Name] -> Q Exp
unpackInstanceE res ctor row fields = do
	boundFields <- mapM (bindColumnS res row) fields
	unboundConstruction <- constructRecordE ctor fields
	pure (DoE (boundFields ++ [NoBindS unboundConstruction]))

-- | Generate an expression which traverses all rows in order to unpack table instances from them.
unpackRowsE :: Name -> Name -> Name -> [Name] -> Q Exp
unpackRowsE res table ctor fields =
	[e| do rows <- lift (P.ntuples $(varE res))
	       idNfo <- $(identColumnInfo res table)
	       forM [0 .. rows - 1] $ \ row ->
	           Resolved <$> $(unpackIdentColumnE res 'row 'idNfo)
	                    <*> $(unpackInstanceE res ctor 'row fields) |]

-- | Generate an expression which retrieves a table instance from each row.
fromResultE :: Name -> Name -> Name -> [Name] -> Q Exp
fromResultE res table ctor fields = do
	infoBinds <- mapM (bindColumnInfoS res) fields
	rowTraversal <- unpackRowsE res table ctor fields
	pure (DoE (infoBinds ++ [NoBindS rowTraversal]))

-- | Implement an instance 'Table' for the given type.
implementTableD :: Name -> Name -> [(Name, Type)] -> Q [Dec]
implementTableD table ctor fields =
	[d| instance Table $(pure (ConT table)) where
	        insertStatement row =
	            Statement {
	                statementContent = $(insertStatementE table fieldNames),
	                statementParams  = $(packParamsE 'row fieldNames)
	            }

	        updateStatement (Reference rid) =
	        	Statement {
	                statementContent = $(emptyUpdateStatementE table),
	                statementParams  = [pack rid]
	            }
	        updateStatement (Resolved rid row) =
	            Statement {
	                statementContent = $(updateStatementE table (length fieldNames)),
	                statementParams  = pack rid : $(packParamsE 'row fieldNames)
	            }

	        deleteStatement ref =
	            Statement {
	                statementContent = $(deleteStatementE table),
	                statementParams  = [pack (referenceID ref)]
	            }

	        createStatement _ =
	            Statement {
	                statementContent = $(createStatementE table fields),
	                statementParams  = []
	            }

	        dropStatement _ =
	            Statement {
	                statementContent = $(dropStatementE table),
	                statementParams  = []
	            }

	        fromResult res =
	            $(fromResultE 'res table ctor fieldNames) |]
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

-- | Inline the create table statement of a table.
mkCreateStatement :: Name -> Q Exp
mkCreateStatement name =
	[e| createStatement (Proxy :: Proxy $(pure (ConT name))) |]

-- | Inline the drop table statement of a table.
mkDropStatement :: Name -> Q Exp
mkDropStatement name =
	[e| dropStatement (Proxy :: Proxy $(pure (ConT name))) |]
