{-# LANGUAGE TemplateHaskell #-}

module Database.PostgreSQL.Store.TH (
	mkTable,
	mkCreateQuery,
	mkDropQuery,
	pgsq
) where

import           Control.Monad
import           Control.Applicative
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Data.List
import           Data.String
import           Data.Typeable
import           Text.Parsec hiding ((<|>), many)
import           Database.PostgreSQL.Store.Types
import qualified Database.PostgreSQL.LibPQ as P
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote

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
			    show (columnTypeDescription :: ColumnTypeDescription $(pure ftype)) |]

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

-- | Generate a query which binds information about a column to the column's info name.
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

-- | Generate a query which binds the unpacked data for a column at a given row to the column's name.
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

	        updateQuery (Reference rid) =
	        	Query {
	                queryStatement = $(emptyUpdateQueryE table),
	                queryParams    = [pack rid]
	            }
	        updateQuery (Resolved rid row) =
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

	        fromResult res =
	            $(fromResultE 'res table ctor fieldNames)

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

-- | PG Store Query
pgsq :: QuasiQuoter
pgsq =
	QuasiQuoter {
		quoteExp  = parseStoreQueryE,
		quotePat  = const (error "Cannot use 'pgsq' in pattern"),
		quoteType = const (error "Cannot use 'pgsq' in type"),
		quoteDec  = const (error "Cannot use 'pgsq' in declaration")
	}

-- |
data QState = QState Int [Exp]

-- |
type QParser = ParsecT String QState Q

-- |
name :: QParser String
name =
	(:) <$> (letter <|> char '_')
	    <*> many (alphaNum <|> char '_')

-- |
entity :: QParser String
entity = do
	strName <- name
	mbTName <- lift (lookupTypeName strName)
	mbVName <- lift (lookupValueName strName)
	pure $ case mbTName <|> mbVName of
		Just name -> sanitizeName name
		Nothing   -> strName

-- |
variable :: QParser String
variable = do
	char '$'
	strName <- name

	paramExp <- lift $ do
		mbName <- lookupValueName strName
		name <- maybe (fail ("Cannot find value '" ++ strName ++ "'")) pure mbName
		[e| pack $(varE name) |]

	QState numParams params <- getState
	putState (QState (numParams + 1) (params ++ [paramExp]))

	pure ('$' : show (numParams + 1))

-- |
quasiQuotationParser :: Loc -> QParser Exp
quasiQuotationParser loc = do
	let (line, column) = loc_start loc
	pos <- getPosition
	setPosition (setSourceLine (setSourceColumn pos column) line)

	contents <- many (variable <|> entity <|> ((: []) <$> anyChar))
	eof

	QState _ params <- getState
	lift $
		[e| Query {
		        queryStatement = fromString $(stringE (concat contents)),
		        queryParams    = $(pure (ListE params))
		    } |]

-- |
parseStoreQueryE :: String -> Q Exp
parseStoreQueryE code = do
	loc <- location
	either (fail . show) pure =<< runParserT (quasiQuotationParser loc) (QState 0 []) (loc_filename loc) code
