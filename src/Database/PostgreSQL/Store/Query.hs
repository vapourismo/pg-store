{-# LANGUAGE OverloadedStrings, TemplateHaskell, BangPatterns #-}

module Database.PostgreSQL.Store.Query (
	-- * Tables
	TableDescription (..),
	DescribableTable (..),

	-- * Querying
	Query (..),
	pgsq
) where

import           Language.Haskell.TH
import           Language.Haskell.TH.Quote

import           Control.Applicative
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State

import           Data.Char
import           Data.String
import           Data.Monoid
import           Data.Typeable
import           Data.Attoparsec.Text
import qualified Data.Text as T
import qualified Data.ByteString as B

import           Database.PostgreSQL.Store.Columns

-- | Description of a table type
data TableDescription = TableDescription {
	-- | Table name
	tableName :: String,

	-- | Identifier column name - 'pgsq' does not respect this value when generating row identifiers
	tableIdentColumn :: String
} deriving (Show, Eq, Ord)

-- | Attach meta data to a table type
class DescribableTable a where
	-- | Describe the table.
	describeTable :: Proxy a -> TableDescription
	describeTable proxy =
		TableDescription {
			tableName = describeTableName proxy,
			tableIdentColumn = describeTableIdentifier proxy
		}

	-- | Describe table name.
	describeTableName :: Proxy a -> String
	describeTableName proxy =
		tableName (describeTable proxy)

	-- | Describe table identifier.
	describeTableIdentifier :: Proxy a -> String
	describeTableIdentifier proxy =
		tableIdentColumn (describeTable proxy)

-- | Query including statement and parameters.
-- Use the 'pgsq' quasi-quoter to conveniently create queries.
data Query = Query {
	-- | Statement
	queryStatement :: !B.ByteString,

	-- | Parameters
	queryParams :: ![Value]
} deriving (Show, Eq, Ord)

-- | Generate a 'Query' from a SQL statement.
--
-- = Table and column names
--
-- All plain identifiers will be treated as Haskell names. They are going to be resolved to their
-- fully-qualified and quoted version. Beware, the use of names which don't refer to a table type
-- or field will likely result in unknown table or column errors. If you don't want a name to be
-- resolved use a quoted identifier.
--
-- Example:
--
-- @
-- module MyModule where
--
-- ...
--
-- data Table = Table { myField :: Int }
-- 'mkTable' ''Table
--
-- myQuery :: 'Query'
-- myQuery = ['pgsq'| SELECT * FROM Table WHERE myField > 1337 |]
-- @
--
-- The SQL statement associated with @myQuery@ will be:
--
-- > SELECT * FROM "MyModule.Table" WHERE "MyModule.myField" > 1337
--
-- = Variables
--
-- You can use reference variables with @$myVariable@. The variable's type has to be an instance of
-- 'Column', otherwise it cannot be attached as query parameter.
--
-- Example:
--
-- @
-- magicNumber :: Int
-- magicNumber = 1337
--
-- myQuery :: 'Query'
-- myQuery = ['pgsq'| SELECT * FROM Table WHERE myField > $magicNumber |]
-- @
--
-- = Row identifiers
--
-- Each instance of @('Table' a) => 'Row' a@ and each row of the actual table inside the database
-- has an identifier value. These identifiers are used to reference specific rows. The identifier
-- column is exposed via the @&MyTable@ pattern.
--
-- Example:
--
-- @
-- ['pgsq'| SELECT *
--        FROM TableA, TableB
--        WHERE refToB = &TableB |]
-- @
--
-- Note @refToB@ is a field of @TableA@.
-- In different circumstances one would write such query as follows.
--
-- > SELECT *
-- > FROM TableA a, Table b
-- > WHERE a.refToB = b.id
--
pgsq :: QuasiQuoter
pgsq =
	QuasiQuoter {
		quoteExp  = parseStoreQueryE,
		quotePat  = const (fail "Cannot use 'pgsq' in pattern"),
		quoteType = const (fail "Cannot use 'pgsq' in type"),
		quoteDec  = const (fail "Cannot use 'pgsq' in declaration")
	}

-- | List of relevant SQL keywords
reservedSQLKeywords :: [T.Text]
reservedSQLKeywords =
	["ABS", "ABSOLUTE", "ACTION", "ADD", "ALL", "ALLOCATE", "ALTER", "ANALYSE", "ANALYZE", "AND",
	 "ANY", "ARE", "ARRAY", "ARRAY_AGG", "ARRAY_MAX_CARDINALITY", "AS", "ASC", "ASENSITIVE",
	 "ASSERTION", "ASYMMETRIC", "AT", "ATOMIC", "AUTHORIZATION", "AVG", "BEGIN", "BEGIN_FRAME",
	 "BEGIN_PARTITION", "BETWEEN", "BIGINT", "BINARY", "BIT", "BIT_LENGTH", "BLOB", "BOOLEAN",
	 "BOTH", "BY", "CALL", "CALLED", "CARDINALITY", "CASCADE", "CASCADED", "CASE", "CAST", "CATALOG",
	 "CEIL", "CEILING", "CHAR", "CHARACTER", "CHARACTER_LENGTH", "CHAR_LENGTH", "CHECK", "CLOB",
	 "CLOSE", "COALESCE", "COLLATE", "COLLATION", "COLLECT", "COLUMN", "COMMIT", "CONCURRENTLY",
	 "CONDITION", "CONNECT", "CONNECTION", "CONSTRAINT", "CONSTRAINTS", "CONTAINS", "CONTINUE",
	 "CONVERT", "CORR", "CORRESPONDING", "COUNT", "COVAR_POP", "COVAR_SAMP", "CREATE", "CROSS",
	 "CUBE", "CUME_DIST", "CURRENT", "CURRENT_CATALOG", "CURRENT_DATE",
	 "CURRENT_DEFAULT_TRANSFORM_GROUP", "CURRENT_PATH", "CURRENT_ROLE",
	 "CURRENT_ROW", "CURRENT_SCHEMA", "CURRENT_TIME", "CURRENT_TIMESTAMP",
	 "CURRENT_TRANSFORM_GROUP_FOR_TYPE", "CURRENT_USER", "CURSOR", "CYCLE", "DATALINK", "DATE",
	 "DAY", "DEALLOCATE", "DEC", "DECIMAL", "DECLARE", "DEFAULT", "DEFERRABLE", "DEFERRED", "DELETE",
	 "DENSE_RANK", "DEREF", "DESC", "DESCRIBE", "DESCRIPTOR", "DETERMINISTIC", "DIAGNOSTICS",
	 "DISCONNECT", "DISTINCT", "DLNEWCOPY", "DLPREVIOUSCOPY", "DLURLCOMPLETE", "DLURLCOMPLETEONLY",
	 "DLURLCOMPLETEWRITE", "DLURLPATH", "DLURLPATHONLY", "DLURLPATHWRITE", "DLURLSCHEME",
	 "DLURLSERVER", "DLVALUE", "DO", "DOMAIN", "DOUBLE", "DROP", "DYNAMIC", "EACH", "ELEMENT",
	 "ELSE", "END", "END", "END_FRAME", "END_PARTITION", "EQUALS", "ESCAPE", "EVERY", "EXCEPT",
	 "EXCEPTION", "EXEC", "EXECUTE", "EXISTS", "EXP", "EXTERNAL", "EXTRACT", "FALSE", "FETCH",
	 "FILTER", "FIRST", "FIRST_VALUE", "FLOAT", "FLOOR", "FOR", "FOREIGN", "FOUND", "FRAME_ROW",
	 "FREE", "FREEZE", "FROM", "FULL", "FUNCTION", "FUSION", "GET", "GLOBAL", "GO", "GOTO", "GRANT",
	 "GROUP", "GROUPING", "GROUPS", "HAVING", "HOLD", "HOUR", "IDENTITY", "ILIKE", "IMMEDIATE",
	 "IMPORT", "IN", "INDICATOR", "INITIALLY", "INNER", "INOUT", "INPUT", "INSENSITIVE", "INSERT",
	 "INT", "INTEGER", "INTERSECT", "INTERSECTION", "INTERVAL", "INTO", "IS", "ISNULL", "ISOLATION",
	 "JOIN", "KEY", "LAG", "LANGUAGE", "LARGE", "LAST", "LAST_VALUE", "LATERAL", "LEAD", "LEADING",
	 "LEFT", "LEVEL", "LIKE", "LIKE_REGEX", "LIMIT", "LN", "LOCAL", "LOCALTIME", "LOCALTIMESTAMP",
	 "LOWER", "MATCH", "MAX", "MAX_CARDINALITY", "MEMBER", "MERGE", "METHOD", "MIN", "MINUTE", "MOD",
	 "MODIFIES", "MODULE", "MONTH", "MULTISET", "NAMES", "NATIONAL", "NATURAL", "NCHAR", "NCLOB",
	 "NEW", "NEXT", "NO", "NONE", "NORMALIZE", "NOT", "NOTNULL", "NTH_VALUE", "NTILE", "NULL",
	 "NULLIF", "NUMERIC", "OCCURRENCES_REGEX", "OCTET_LENGTH", "OF", "OFFSET", "OLD", "ON", "ONLY",
	 "OPEN", "OPTION", "OR", "ORDER", "OUT", "OUTER", "OUTPUT", "OVER", "OVERLAPS", "OVERLAY", "PAD",
	 "PARAMETER", "PARTIAL", "PARTITION", "PERCENT", "PERCENTILE_CONT", "PERCENTILE_DISC",
	 "PERCENT_RANK", "PERIOD", "PLACING", "PORTION", "POSITION", "POSITION_REGEX", "POWER",
	 "PRECEDES", "PRECISION", "PREPARE", "PRESERVE", "PRIMARY", "PRIOR", "PRIVILEGES", "PROCEDURE",
	 "PUBLIC", "RANGE", "RANK", "READ", "READS", "REAL", "RECURSIVE", "REF", "REFERENCES",
	 "REFERENCING", "REGR_AVGX", "REGR_AVGY", "REGR_COUNT", "REGR_INTERCEPT", "REGR_R2",
	 "REGR_SLOPE", "REGR_SXX", "REGR_SXY", "REGR_SYY", "RELATIVE", "RELEASE", "RESTRICT", "RESULT",
	 "RETURN", "RETURNING", "RETURNS", "REVOKE", "RIGHT", "ROLLBACK", "ROLLUP", "ROW", "ROWS",
	 "ROW_NUMBER", "SAVEPOINT", "SCHEMA", "SCOPE", "SCROLL", "SEARCH", "SECOND", "SECTION", "SELECT",
	 "SENSITIVE", "SESSION", "SESSION_USER", "SET", "SIMILAR", "SIZE", "SMALLINT", "SOME", "SPACE",
	 "SPECIFIC", "SPECIFICTYPE", "SQL", "SQLCODE", "SQLERROR", "SQLEXCEPTION", "SQLSTATE",
	 "SQLWARNING", "SQRT", "START", "STATIC", "STDDEV_POP", "STDDEV_SAMP", "SUBMULTISET",
	 "SUBSTRING", "SUBSTRING_REGEX", "SUCCEEDS", "SUM", "SYMMETRIC", "SYSTEM", "SYSTEM_TIME",
	 "SYSTEM_USER", "TABLE", "TABLESAMPLE", "TEMPORARY", "THEN", "TIME", "TIMESTAMP",
	 "TIMEZONE_HOUR", "TIMEZONE_MINUTE", "TO", "TRAILING", "TRANSACTION", "TRANSLATE",
	 "TRANSLATE_REGEX", "TRANSLATION", "TREAT", "TRIGGER", "TRIM", "TRIM_ARRAY", "TRUE", "TRUNCATE",
	 "UESCAPE", "UNION", "UNIQUE", "UNKNOWN", "UNNEST", "UPDATE", "UPPER", "USAGE", "USER", "USING",
	 "VALUE", "VALUES", "VALUE_OF", "VARBINARY", "VARCHAR", "VARIADIC", "VARYING", "VAR_POP",
	 "VAR_SAMP", "VERBOSE", "VERSIONING", "VIEW", "WHEN", "WHENEVER", "WHERE", "WIDTH_BUCKET",
	 "WINDOW", "WITH", "WITHIN", "WITHOUT", "WORK", "WRITE", "XML", "XMLAGG", "XMLATTRIBUTES",
	 "XMLBINARY", "XMLCAST", "XMLCOMMENT", "XMLCONCAT", "XMLDOCUMENT", "XMLELEMENT", "XMLEXISTS",
	 "XMLFOREST", "XMLITERATE", "XMLNAMESPACES", "XMLPARSE", "XMLPI", "XMLQUERY", "XMLSERIALIZE",
	 "XMLTABLE", "XMLTEXT", "XMLVALIDATE", "YEAR", "ZONE"]

-- | Query segment
data Segment
	= Keyword T.Text
	| PossibleName T.Text
	| Variable T.Text
	| Identifier T.Text
	| Quote Char T.Text
	| Other Char

-- | SQL keyword
keyword :: Parser Segment
keyword =
	Keyword <$> choice (asciiCI <$> reservedSQLKeywords)

-- | Alpha numeric character
alphaNum :: Parser Char
alphaNum = satisfy isAlphaNum

-- | Underscore
underscore :: Parser Char
underscore = char '_'

-- | Dot
dot :: Parser Char
dot = char '.'

-- | Name
name :: Parser T.Text
name =
	bake <$> (letter <|> underscore) <*> many (alphaNum <|> underscore <|> dot)
	where
		bake h t = T.pack (h : t)

-- | Possible name
possibleName :: Parser Segment
possibleName =
	PossibleName <$> name

-- | Variable
variable :: Parser Segment
variable = do
	char '$'
	Variable <$> name

-- | Identifier
identifier :: Parser Segment
identifier = do
	char '&'
	Identifier <$> name

-- | Quote
quote :: Char -> Parser Segment
quote delim = do
	char delim
	cnt <- scan (False, T.empty) scanner
	char delim
	pure (Quote delim cnt)
	where
		scanner (False, _) chr | chr == delim = Nothing
		scanner (esc, cnt) chr = Just (not esc && chr == '\\', cnt `T.snoc` chr)

-- | Segments
segments :: Parser [Segment]
segments =
	many (choice [
		quote '"',
		quote '\'',
		variable,
		identifier,
		keyword,
		possibleName,
		Other <$> anyChar
	])

-- | Reduce segments in order to resolve names and collect query parameters.
reduceSegment :: Segment -> StateT (Int, [Exp]) Q T.Text
reduceSegment seg =
	case seg of
		Keyword kw ->
			pure kw

		Other o ->
			pure (T.singleton o)

		Quote delim cnt ->
			pure (T.singleton delim <> cnt <> T.singleton delim)

		Variable varName -> do
			mbName <- lift (lookupValueName (T.unpack varName))
			case mbName of
				Just name -> do
					-- Generate the pack expression
					lit <- lift [e| pack $(varE name) |]

					-- Register parameter
					(numParams, params) <- get
					put (numParams + 1, params ++ [lit])

					pure (T.pack ("$" ++ show (numParams + 1)))

				Nothing ->
					lift (fail ("\ESC[34m" ++ T.unpack varName ++ "\ESC[0m does not refer to anything"))

		PossibleName posName -> do
			let strName = T.unpack posName
			mbName <- lift ((<|>) <$> lookupTypeName strName <*> lookupValueName strName)
			pure $ case mbName of
				Just name ->
					T.pack (sanitizeName' name)

				Nothing ->
					posName

		Identifier idnName -> do
			mbName <- lift (lookupTypeName (T.unpack idnName))
			case mbName of
				Just name ->
					pure (T.pack (identField' name))

				Nothing ->
					lift (fail ("\ESC[34m" ++ T.unpack idnName ++ "\ESC[0m does not refer to anything"))

-- | Parse quasi-quoted PG Store Query.
parseStoreQueryE :: String -> Q Exp
parseStoreQueryE code = do
	case parseOnly segments (fromString code) of
		Left msg ->
			fail msg

		Right xs -> do
			(statement, (_, params)) <- runStateT (mapM reduceSegment xs) (0, [])
			[e| Query {
			        queryStatement = fromString $(stringE (T.unpack (T.concat statement))),
			        queryParams    = $(pure (ListE params))
			    } |]
