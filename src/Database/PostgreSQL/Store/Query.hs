{-# LANGUAGE OverloadedStrings, TemplateHaskell, BangPatterns #-}

-- |
-- Module:     Database.PostgreSQL.Store.Query
-- Copyright:  (c) Ole Krüger 2015-2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.Query (
	Query (..)
) where

--import           Language.Haskell.TH
--import           Language.Haskell.TH.Quote

--import           Control.Applicative
--import           Control.Monad.Trans.Class
--import           Control.Monad.Trans.State

--import           Data.Char
--import           Data.String
--import           Data.Typeable
--import           Data.Attoparsec.Text
--import qualified Data.Text as T
--import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B

import           Database.PostgreSQL.Store.Columns

---- | Description of a table type
--data TableDescription = TableDescription {
--	-- | Table name
--	tableName :: String,

--	-- | Identifier column name
--	tableIdentifier :: String
--} deriving (Show, Eq, Ord)

---- | Attach meta data to a table type
--class DescribableTable a where
--	-- | Describe the table.
--	describeTable :: Proxy a -> TableDescription
--	describeTable proxy =
--		TableDescription {
--			tableName = describeTableName proxy,
--			tableIdentifier = describeTableIdentifier proxy
--		}

--	-- | Describe table name.
--	describeTableName :: Proxy a -> String
--	describeTableName proxy =
--		tableName (describeTable proxy)

--	-- | Describe table identifier.
--	describeTableIdentifier :: Proxy a -> String
--	describeTableIdentifier proxy =
--		tableIdentifier (describeTable proxy)

--	{-# MINIMAL (describeTableName, describeTableIdentifier) | describeTable #-}

-- | Query including statement and parameters.
-- Use the 'pgsq' quasi-quoter to conveniently create queries.
data Query = Query {
	-- | Statement
	queryStatement :: !B.ByteString,

	-- | Parameters
	queryParams :: ![Value]
} deriving (Show, Eq, Ord)

---- | Generate a 'Query' from a SQL statement.
----
---- = Table and column names
----
---- All plain identifiers will be treated as Haskell names. They are going to be resolved to their
---- fully-qualified and quoted version. Beware, the use of names which don't refer to a table type
---- or field will likely result in unknown table or column errors. The associated table name of a
---- type is retrieved using 'describeTableName'.
---- If you don't want a name to be resolved use a quoted identifier.
---- SQL keywords are not treated as names. If your table or column name is also a SQL keyword, simply
---- prefix the name with an @\@@.
----
---- Example:
----
---- @
---- {-\# LANGUAGE QuasiQuotes \#-}
---- module MyModule where
----
---- ...
----
---- data Table = Table { myField :: Int }
---- 'mkTable' ''Table []
----
---- myQuery :: 'Query'
---- myQuery = ['pgsq'| SELECT * FROM Table WHERE myField > 1337 |]
---- @
----
---- The SQL statement associated with @myQuery@ will be:
----
---- > SELECT * FROM "MyModule.Table" WHERE "MyModule.myField" > 1337
----
---- = Variables
----
---- You can use reference variables with @$myVariable@. The variable's type has to be an instance of
---- 'Column', otherwise it cannot be attached as query parameter.
----
---- Example:
----
---- @
---- magicNumber :: Int
---- magicNumber = 1337
----
---- myQuery :: 'Query'
---- myQuery = ['pgsq'| SELECT * FROM Table WHERE myField > $magicNumber |]
---- @
----
---- = Row identifiers
----
---- Each instance of @('Table' a) => 'Row' a@, @('Table' a) => 'Reference' a@ and each row of the actual table inside the database
---- has an identifier value. These identifiers are used to reference specific rows. The identifier
---- column is exposed via the @&MyTable@ pattern. Identifier field names are resolved using
---- 'describeTableIdentifier'.
----
---- Example:
----
---- @
---- data TableB = TableB {
----     b :: Int
---- }
----
---- 'mkTable' ''TableB []
----
---- data TableA = TableA {
----     a       :: Int,
----     refToB  :: Reference TableB
---- }
----
---- 'mkTable' ''TableA []
----
---- ...
----
---- ['pgsq'| SELECT *
----        FROM TableA, TableB
----        WHERE refToB = &TableB |]
---- @
----
---- The resulting SQL statement works similar to the following.
----
---- > SELECT *
---- > FROM TableA a, TableB b
---- > WHERE a.refToB = b.id
----
--pgsq :: QuasiQuoter
--pgsq =
--	QuasiQuoter {
--		quoteExp  = parseStoreQueryE,
--		quotePat  = const (fail "Cannot use 'pgsq' in pattern"),
--		quoteType = const (fail "Cannot use 'pgsq' in type"),
--		quoteDec  = const (fail "Cannot use 'pgsq' in declaration")
--	}

---- | List of relevant SQL keywords
--reservedSQLKeywords :: [T.Text]
--reservedSQLKeywords =
--	["ABS", "ABSOLUTE", "ACTION", "ADD", "ALL", "ALLOCATE", "ALTER", "ANALYSE", "ANALYZE", "AND",
--	 "ANY", "ARE", "ARRAY", "ARRAY_AGG", "ARRAY_MAX_CARDINALITY", "AS", "ASC", "ASENSITIVE",
--	 "ASSERTION", "ASYMMETRIC", "AT", "ATOMIC", "AUTHORIZATION", "AVG", "BEGIN", "BEGIN_FRAME",
--	 "BEGIN_PARTITION", "BETWEEN", "BIGINT", "BINARY", "BIT", "BIT_LENGTH", "BLOB", "BOOLEAN",
--	 "BOTH", "BY", "CALL", "CALLED", "CARDINALITY", "CASCADE", "CASCADED", "CASE", "CAST", "CATALOG",
--	 "CEIL", "CEILING", "CHAR", "CHARACTER", "CHARACTER_LENGTH", "CHAR_LENGTH", "CHECK", "CLOB",
--	 "CLOSE", "COALESCE", "COLLATE", "COLLATION", "COLLECT", "COLUMN", "COMMIT", "CONCURRENTLY",
--	 "CONDITION", "CONNECT", "CONNECTION", "CONSTRAINT", "CONSTRAINTS", "CONTAINS", "CONTINUE",
--	 "CONVERT", "CORR", "CORRESPONDING", "COUNT", "COVAR_POP", "COVAR_SAMP", "CREATE", "CROSS",
--	 "CUBE", "CUME_DIST", "CURRENT", "CURRENT_CATALOG", "CURRENT_DATE",
--	 "CURRENT_DEFAULT_TRANSFORM_GROUP", "CURRENT_PATH", "CURRENT_ROLE",
--	 "CURRENT_ROW", "CURRENT_SCHEMA", "CURRENT_TIME", "CURRENT_TIMESTAMP",
--	 "CURRENT_TRANSFORM_GROUP_FOR_TYPE", "CURRENT_USER", "CURSOR", "CYCLE", "DATALINK", "DATE",
--	 "DAY", "DEALLOCATE", "DEC", "DECIMAL", "DECLARE", "DEFAULT", "DEFERRABLE", "DEFERRED", "DELETE",
--	 "DENSE_RANK", "DEREF", "DESC", "DESCRIBE", "DESCRIPTOR", "DETERMINISTIC", "DIAGNOSTICS",
--	 "DISCONNECT", "DISTINCT", "DLNEWCOPY", "DLPREVIOUSCOPY", "DLURLCOMPLETE", "DLURLCOMPLETEONLY",
--	 "DLURLCOMPLETEWRITE", "DLURLPATH", "DLURLPATHONLY", "DLURLPATHWRITE", "DLURLSCHEME",
--	 "DLURLSERVER", "DLVALUE", "DO", "DOMAIN", "DOUBLE", "DROP", "DYNAMIC", "EACH", "ELEMENT",
--	 "ELSE", "END", "END", "END_FRAME", "END_PARTITION", "EQUALS", "ESCAPE", "EVERY", "EXCEPT",
--	 "EXCEPTION", "EXEC", "EXECUTE", "EXISTS", "EXP", "EXTERNAL", "EXTRACT", "FALSE", "FETCH",
--	 "FILTER", "FIRST", "FIRST_VALUE", "FLOAT", "FLOOR", "FOR", "FOREIGN", "FOUND", "FRAME_ROW",
--	 "FREE", "FREEZE", "FROM", "FULL", "FUNCTION", "FUSION", "GET", "GLOBAL", "GO", "GOTO", "GRANT",
--	 "GROUP", "GROUPING", "GROUPS", "HAVING", "HOLD", "HOUR", "IDENTITY", "ILIKE", "IMMEDIATE",
--	 "IMPORT", "IN", "INDICATOR", "INITIALLY", "INNER", "INOUT", "INPUT", "INSENSITIVE", "INSERT",
--	 "INT", "INTEGER", "INTERSECT", "INTERSECTION", "INTERVAL", "INTO", "IS", "ISNULL", "ISOLATION",
--	 "JOIN", "KEY", "LAG", "LANGUAGE", "LARGE", "LAST", "LAST_VALUE", "LATERAL", "LEAD", "LEADING",
--	 "LEFT", "LEVEL", "LIKE", "LIKE_REGEX", "LIMIT", "LN", "LOCAL", "LOCALTIME", "LOCALTIMESTAMP",
--	 "LOWER", "MATCH", "MAX", "MAX_CARDINALITY", "MEMBER", "MERGE", "METHOD", "MIN", "MINUTE", "MOD",
--	 "MODIFIES", "MODULE", "MONTH", "MULTISET", "NAMES", "NATIONAL", "NATURAL", "NCHAR", "NCLOB",
--	 "NEW", "NEXT", "NO", "NONE", "NORMALIZE", "NOT", "NOTNULL", "NTH_VALUE", "NTILE", "NULL",
--	 "NULLIF", "NUMERIC", "OCCURRENCES_REGEX", "OCTET_LENGTH", "OF", "OFFSET", "OLD", "ON", "ONLY",
--	 "OPEN", "OPTION", "OR", "ORDER", "OUT", "OUTER", "OUTPUT", "OVER", "OVERLAPS", "OVERLAY", "PAD",
--	 "PARAMETER", "PARTIAL", "PARTITION", "PERCENT", "PERCENTILE_CONT", "PERCENTILE_DISC",
--	 "PERCENT_RANK", "PERIOD", "PLACING", "PORTION", "POSITION", "POSITION_REGEX", "POWER",
--	 "PRECEDES", "PRECISION", "PREPARE", "PRESERVE", "PRIMARY", "PRIOR", "PRIVILEGES", "PROCEDURE",
--	 "PUBLIC", "RANGE", "RANK", "READ", "READS", "REAL", "RECURSIVE", "REF", "REFERENCES",
--	 "REFERENCING", "REGR_AVGX", "REGR_AVGY", "REGR_COUNT", "REGR_INTERCEPT", "REGR_R2",
--	 "REGR_SLOPE", "REGR_SXX", "REGR_SXY", "REGR_SYY", "RELATIVE", "RELEASE", "RESTRICT", "RESULT",
--	 "RETURN", "RETURNING", "RETURNS", "REVOKE", "RIGHT", "ROLLBACK", "ROLLUP", "ROW", "ROWS",
--	 "ROW_NUMBER", "SAVEPOINT", "SCHEMA", "SCOPE", "SCROLL", "SEARCH", "SECOND", "SECTION", "SELECT",
--	 "SENSITIVE", "SESSION", "SESSION_USER", "SET", "SIMILAR", "SIZE", "SMALLINT", "SOME", "SPACE",
--	 "SPECIFIC", "SPECIFICTYPE", "SQL", "SQLCODE", "SQLERROR", "SQLEXCEPTION", "SQLSTATE",
--	 "SQLWARNING", "SQRT", "START", "STATIC", "STDDEV_POP", "STDDEV_SAMP", "SUBMULTISET",
--	 "SUBSTRING", "SUBSTRING_REGEX", "SUCCEEDS", "SUM", "SYMMETRIC", "SYSTEM", "SYSTEM_TIME",
--	 "SYSTEM_USER", "TABLE", "TABLESAMPLE", "TEMPORARY", "THEN", "TIME", "TIMESTAMP",
--	 "TIMEZONE_HOUR", "TIMEZONE_MINUTE", "TO", "TRAILING", "TRANSACTION", "TRANSLATE",
--	 "TRANSLATE_REGEX", "TRANSLATION", "TREAT", "TRIGGER", "TRIM", "TRIM_ARRAY", "TRUE", "TRUNCATE",
--	 "UESCAPE", "UNION", "UNIQUE", "UNKNOWN", "UNNEST", "UPDATE", "UPPER", "USAGE", "USER", "USING",
--	 "VALUE", "VALUES", "VALUE_OF", "VARBINARY", "VARCHAR", "VARIADIC", "VARYING", "VAR_POP",
--	 "VAR_SAMP", "VERBOSE", "VERSIONING", "VIEW", "WHEN", "WHENEVER", "WHERE", "WIDTH_BUCKET",
--	 "WINDOW", "WITH", "WITHIN", "WITHOUT", "WORK", "WRITE", "XML", "XMLAGG", "XMLATTRIBUTES",
--	 "XMLBINARY", "XMLCAST", "XMLCOMMENT", "XMLCONCAT", "XMLDOCUMENT", "XMLELEMENT", "XMLEXISTS",
--	 "XMLFOREST", "XMLITERATE", "XMLNAMESPACES", "XMLPARSE", "XMLPI", "XMLQUERY", "XMLSERIALIZE",
--	 "XMLTABLE", "XMLTEXT", "XMLVALIDATE", "YEAR", "ZONE"]

---- | Query segment
--data Segment
--	= Keyword T.Text
--	| PossibleName T.Text
--	| Variable T.Text
--	| Identifier T.Text
--	| Quote Char T.Text
--	| Other Char

---- | Alpha numeric character
--alphaNum :: Parser Char
--alphaNum = satisfy isAlphaNum

---- | Underscore
--underscore :: Parser Char
--underscore = char '_'

---- | Dot
--dot :: Parser Char
--dot = char '.'

---- | Name
--name :: Parser T.Text
--name =
--	bake <$> (letter <|> underscore) <*> many (alphaNum <|> underscore <|> dot)
--	where
--		bake h t = T.pack (h : t)

---- | Possible name
--possibleName :: Parser Segment
--possibleName = do
--	n <- name
--	if elem (T.toUpper n) reservedSQLKeywords then
--		pure (Keyword n)
--	else
--		pure (PossibleName n)

---- | Explicit name
--explicitName :: Parser Segment
--explicitName = do
--	char '@'
--	PossibleName <$> name

---- | Variable
--variable :: Parser Segment
--variable = do
--	char '$'
--	Variable <$> name

---- | Identifier
--identifier :: Parser Segment
--identifier = do
--	char '&'
--	Identifier <$> name

---- | Quote
--quote :: Char -> Parser Segment
--quote delim = do
--	char delim
--	cnt <- scan (False, T.empty) scanner
--	char delim
--	pure (Quote delim cnt)
--	where
--		scanner (False, _) chr | chr == delim = Nothing
--		scanner (esc, cnt) chr = Just (not esc && chr == '\\', cnt `T.snoc` chr)

---- | Segments
--segments :: Parser [Segment]
--segments =
--	many (choice [
--		quote '"',
--		quote '\'',
--		variable,
--		identifier,
--		explicitName,
--		possibleName,
--		Other <$> anyChar
--	])

---- | Turn "Text" into a "String" expression.
--textE :: T.Text -> StateT (Int, [Exp]) Q Exp
--textE txt =
--	lift (stringE (T.unpack txt))

---- | Reduce segments in order to resolve names and collect query parameters.
--reduceSegment :: Segment -> StateT (Int, [Exp]) Q Exp
--reduceSegment seg =
--	case seg of
--		Keyword kw ->
--			textE kw

--		Other o ->
--			lift (stringE [o])

--		Quote delim cnt ->
--			lift (stringE (delim : T.unpack cnt ++ [delim]))

--		Variable varName -> do
--			mbName <- lift (lookupValueName (T.unpack varName))
--			case mbName of
--				Just name -> do
--					-- Generate the pack expression
--					lit <- lift [e| pack $(varE name) |]

--					-- Register parameter
--					(numParams, params) <- get
--					put (numParams + 1, params ++ [lit])

--					lift (stringE ("$" ++ show (numParams + 1)))

--				Nothing ->
--					lift (fail ("\ESC[34m" ++ T.unpack varName ++
--					            "\ESC[0m does not refer to anything"))

--		PossibleName posName -> do
--			let strName = T.unpack posName
--			mbName <- lift ((,) <$> lookupTypeName strName <*> lookupValueName strName)
--			case mbName of
--				(Just typName, _) ->
--					lift [e| "\"" ++ describeTableName (Proxy :: Proxy $(conT typName)) ++ "\"" |]

--				(_, Just varName) ->
--					lift (stringE (sanitizeName' varName))

--				_ -> textE posName

--		Identifier idnName -> do
--			mbName <- lift (lookupTypeName (T.unpack idnName))
--			case mbName of
--				Just name ->
--					lift [e| "\"" ++ describeTableIdentifier (Proxy :: Proxy $(conT name)) ++ "\"" |]

--				Nothing ->
--					lift (fail ("\ESC[34m" ++ T.unpack idnName ++
--					            "\ESC[0m does not refer to anything"))

---- | Parse quasi-quoted PG Store Query.
--parseStoreQueryE :: String -> Q Exp
--parseStoreQueryE code = do
--	case parseOnly segments (fromString code) of
--		Left msg ->
--			fail msg

--		Right xs -> do
--			(parts, (_, params)) <- runStateT (mapM reduceSegment xs) (0, [])
--			[e| Query {
--			        queryStatement = T.encodeUtf8 (T.pack (concat $(pure (ListE parts)))),
--			        queryParams    = $(pure (ListE params))
--			    } |]

---- | Similar to the 'pgsq' quasi-quoter but produces only the statement.
--pgss :: QuasiQuoter
--pgss =
--	QuasiQuoter {
--		quoteExp  = parseStoreStatementE,
--		quotePat  = const (fail "Cannot use 'pgss' in pattern"),
--		quoteType = const (fail "Cannot use 'pgss' in type"),
--		quoteDec  = const (fail "Cannot use 'pgss' in declaration")
--	}

---- | Parse quasi-quoted PG Store Statement.
--parseStoreStatementE :: String -> Q Exp
--parseStoreStatementE code = do
--	case parseOnly segments (fromString code) of
--		Left msg ->
--			fail msg

--		Right xs -> do
--			(parts, _) <- runStateT (mapM reduceSegment xs) (0, [])
--			[e| concat $(pure (ListE parts)) |]
