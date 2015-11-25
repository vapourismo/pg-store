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

-- |
columnNumberE :: Name -> Name -> Q Exp
columnNumberE res name =
	[e| MaybeT (P.fnumber $(varE res) (fromString $(stringE (sanitizeName name)))) |]

-- |
idColumnNumberE :: Name -> Name -> Q Exp
idColumnNumberE res table =
	[e| MaybeT (P.fnumber $(varE res) (fromString $(stringE (identField table)))) |]

-- |
columnTypeE :: Name -> Name -> Q Exp
columnTypeE res col =
	[e| lift (P.ftype $(varE res) $(varE col)) |]

-- |
columnFormatE :: Name -> Name -> Q Exp
columnFormatE res col =
	[e| lift (P.fformat $(varE res) $(varE col)) |]

-- |
fieldColumnInfoE :: Name -> Name -> Q Exp
fieldColumnInfoE res name =
	[e| do col <- $(columnNumberE res name)
	       oid <- $(columnTypeE res 'col)
	       fmt <- $(columnFormatE res 'col)
	       pure (col, oid, fmt) |]

-- |
bindFieldInfoS :: Name -> Name -> Name -> Q Stmt
bindFieldInfoS target res name =
	BindS (VarP target) <$> fieldColumnInfoE res name

-- |
unqualifiedFieldName :: Name -> Name
unqualifiedFieldName = mkName . nameBase

-- |
infoFieldName :: Name -> Name
infoFieldName name =
	mkName (nameBase name ++ "_info")

-- |
fieldDataE :: Name -> Name -> Name -> Q Exp
fieldDataE res row col =
	[e| MaybeT (P.getvalue' $(varE res) $(varE row) $(varE col)) |]

-- |
unpackFieldE :: Name -> Name -> Name -> Q Exp
unpackFieldE res row name =
	[e| do let (col, oid, fmt) = $(varE infoName)
	       dat <- $(fieldDataE res row 'col)
	       let val = Value {
	           valueType = oid,
	           valueData = dat,
	           valueFormat = fmt
	       }
	       MaybeT (pure (unpack val)) |]
	where
		infoName = infoFieldName name

-- |
bindFieldS :: Name -> Name -> Name -> Q Stmt
bindFieldS res row name =
	BindS (VarP (unqualifiedFieldName name)) <$> unpackFieldE res row name

-- |
unpackParamsE :: Name -> Name -> Name -> [Name] -> Q Exp
unpackParamsE res table ctor fields = do
	infoBinds <- mapM bindFieldInfo fields
	rowTraversal <- traverseRows
	pure (DoE (infoBinds ++ [NoBindS rowTraversal]))
	where
		bindFieldInfo n =
			bindFieldInfoS (infoFieldName n) res n

		recordConstruction =
			RecConE ctor (map (\ n -> (n, VarE (unqualifiedFieldName n))) fields)

		instanceFromRow row = do
			boundFields <- mapM (bindFieldS res row) fields
			unboundConstruction <- [e| lift (pure $(pure recordConstruction)) |]
			pure (DoE (boundFields ++ [NoBindS unboundConstruction]))

		idInfo =
			[e| do col <- $(idColumnNumberE res table)
			       oid <- $(columnTypeE res 'col)
			       fmt <- $(columnFormatE res 'col)
			       pure (col, oid, fmt) |]

		idFromRow row idNfo =
			[e| do let (col, oid, fmt) = $(varE idNfo)
			       dat <- $(fieldDataE res row 'col)
			       let val = Value {
			           valueType = oid,
			           valueData = dat,
			           valueFormat = fmt
			       }
			       MaybeT (pure (unpack val)) |]

		traverseRows =
			[e| do rows <- lift (P.ntuples $(varE res))
			       idNfo <- $(idInfo)
			       forM [0 .. rows - 1] $ \ row ->
			           Resolved <$> $(idFromRow 'row 'idNfo) <*> $(instanceFromRow 'row) |]

-- | Implement an instance 'Table' for the given type.
implementTable :: Name -> Name -> [(Name, Type)] -> Q [Dec]
implementTable table ctor fields =
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
	            $(unpackParamsE 'res table ctor fieldNames) |]
	where
		fieldNames = map fst fields

-- | Make a type ready to be used as a table.
mkTable :: Name -> Q [Dec]
mkTable name = do
	info <- reify name
	case info of
		TyConI (DataD [] _ [] [RecC ctor records] _) | length records > 0 ->
			implementTable name ctor fields
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
