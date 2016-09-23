{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleInstances, UndecidableInstances #-}

-- |
-- Module:     Database.PostgreSQL.Store.Table.Class
-- Copyright:  (c) Ole Krüger 2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.Query (
	-- * Type
	Query (..),

	-- * Builder
	QueryBuilder,
	buildQuery,

	insertCode,
	insertName,
	QueryEntity (..),
	insertTableName,
	insertTableIdentColumnName,
	insertTableColumnNames,

	-- * Template Haskell
	parseQuery,
	pgsq
) where

import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Language.Haskell.TH.Quote

import           Control.Applicative
import           Control.Monad.State.Strict hiding (lift)

import           Data.List
import           Data.Char
import           Data.Proxy
import           Data.String
import           Data.Attoparsec.Text
import qualified Data.ByteString    as B
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

import           Database.PostgreSQL.Store.Columns
import           Database.PostgreSQL.Store.Table.Class

-- | Query including statement and parameters
data Query = Query {
	-- | Statement
	queryStatement :: !B.ByteString,

	-- | Parameters
	queryParams :: ![Value]
} deriving (Show, Eq, Ord)

-- | Query builder
type QueryBuilder = State (Int, B.ByteString, [Value]) ()

-- | Build a "Query" using the given builder.
buildQuery :: QueryBuilder -> Query
buildQuery builder =
	Query code values
	where
		(_, code, values) = execState builder (1, B.empty, [])

-- | Generate the placeholder for a parameter.
genParam :: Int -> B.ByteString
genParam index =
	B.append "$" (fromString (show index))

-- | Insert a piece of SQL.
insertCode :: B.ByteString -> QueryBuilder
insertCode otherCode =
	modify $ \ (counter, code, values) ->
		(counter, B.append code otherCode, values)

-- | Insert a name. This function takes care of proper quotation.
insertName :: B.ByteString -> QueryBuilder
insertName name =
	insertCode (B.concat ["\"", B.intercalate "\"\"" (B.split 34 name), "\""])

-- | Insert the table name of a table type.
insertTableName :: (Table a) => proxy a -> QueryBuilder
insertTableName proxy =
	insertName (tableName (tableInfo proxy))

-- | Insert the identifier column name of a table type.
insertTableIdentColumnName :: (Table a) => proxy a -> QueryBuilder
insertTableIdentColumnName proxy = do
	insertTableName proxy
	insertCode "."
	insertName (tableIdentColumn (tableInfo proxy))

-- | Insert a comma-seperated list of columns for a table type.
insertTableColumnNames :: (Table a) => proxy a -> QueryBuilder
insertTableColumnNames proxy =
	sequence_ $
		intersperse (insertCode ",") $
			map insertColName (tableColumns (tableInfo proxy))
	where
		insertColName name = do
			insertTableName proxy
			insertCode "."
			insertName name

-- | Generalize over types that can be either inlined or attached.
class QueryEntity a where
	insertEntity :: a -> QueryBuilder

-- | Every type that implements "Column" can be used as entity.
instance {-# OVERLAPPABLE #-} (Column a) => QueryEntity a where
	insertEntity value =
		modify $ \ (counter, code, values) ->
			(counter + 1, B.append code (genParam counter), values ++ [pack value])

-- | List of values are inserted as tuples.
instance (QueryEntity a) => QueryEntity [a] where
	insertEntity xs = do
		insertCode "("
		sequence_ $
			intersperse (insertCode ",") $
				map insertEntity xs
		insertCode ")"

-- | Name
valueName :: Parser String
valueName =
	(:) <$> (letter <|> char '_') <*> many (satisfy isAlphaNum <|> char '_')

-- | Type name
typeName :: Parser String
typeName =
	(:) <$> satisfy isUpper <*> many (satisfy isAlphaNum <|> char '_')

-- | Qualified type name
qualifiedTypeName :: Parser String
qualifiedTypeName =
	intercalate "." <$> sepBy1 typeName (char '.')

-- | Query segment
data QuerySegment
	= QueryTable String
	| QuerySelector String
	| QueryIdentifier String
	| QueryEntity String
	| QueryQuote Char String
	| QueryOther String
	deriving (Show, Eq, Ord)

-- | Table type
tableSegment :: Parser QuerySegment
tableSegment = do
	char '@'
	QueryTable <$> qualifiedTypeName

-- | Segment
selectorSegment :: Parser QuerySegment
selectorSegment = do
	char '#'
	QuerySelector <$> qualifiedTypeName

-- | Identifier
identifierSegment :: Parser QuerySegment
identifierSegment = do
	char '&'
	QueryIdentifier <$> qualifiedTypeName

-- | Entity
entitySegment :: Parser QuerySegment
entitySegment = do
	char '$'
	QueryEntity <$> valueName

-- | Quotation
quoteSegment :: Char -> Parser QuerySegment
quoteSegment delim = do
	char delim
	cnt <- concat <$> many (choice [escapedDelim, notDelim])
	char delim
	pure (QueryQuote delim cnt)
	where
		escapedDelim = char delim >> char delim >> pure [delim, delim]
		notDelim = (: []) <$> notChar delim

-- | Uninterpreted segment
otherSegment :: Parser QuerySegment
otherSegment =
	QueryOther <$> some (satisfy (notInClass "\"'@&#$"))

-- | Segment that is part of the query
querySegment :: Parser QuerySegment
querySegment =
	choice [quoteSegment '\'',
	        quoteSegment '"',
	        tableSegment,
	        selectorSegment,
	        identifierSegment,
	        entitySegment,
	        otherSegment]

-- | Lift "ByteString".
liftByteString :: B.ByteString -> Q Exp
liftByteString bs =
	[e| B.pack $(lift (B.unpack bs)) |]

-- | Translate a "QuerySegment" to an expression.
translateSegment :: QuerySegment -> Q Exp
translateSegment segment =
	case segment of
		QueryTable typeName -> do
			mbTypeName <- lookupTypeName typeName
			case mbTypeName of
				Nothing -> fail ("'" ++ typeName ++ "' does not refer to a type")
				Just typ ->
					[e| insertTableName (Proxy :: Proxy $(conT typ)) |]

		QuerySelector typeName -> do
			mbTypeName <- lookupTypeName typeName
			case mbTypeName of
				Nothing -> fail ("'" ++ typeName ++ "' does not refer to a type")
				Just typ ->
					[e| insertTableColumnNames (Proxy :: Proxy $(conT typ)) |]

		QueryIdentifier typeName -> do
			mbTypeName <- lookupTypeName typeName
			case mbTypeName of
				Nothing -> fail ("'" ++ typeName ++ "' does not refer to a type")
				Just typ ->
					[e| insertTableIdentColumnName (Proxy :: Proxy $(conT typ)) |]

		QueryEntity valueName -> do
			mbValueName <- lookupValueName valueName
			case mbValueName of
				Nothing -> fail ("'" ++ valueName ++ "' does not refer to a type")
				Just valueName ->
					[e| insertEntity $(varE valueName) |]

		QueryQuote _ code ->
			[e| insertCode $(liftByteString (T.encodeUtf8 (T.pack code))) |]

		QueryOther code ->
			[e| insertCode $(liftByteString (T.encodeUtf8 (T.pack code))) |]

-- | Parse a query string.
parseQuery :: String -> Q Exp
parseQuery code =
	case parseOnly (some querySegment <* endOfInput) (T.pack code) of
		Left msg ->
			fail ("Query parser failed: " ++ msg)

		Right segments ->
			[e| buildQuery $(DoE . map NoBindS <$> mapM translateSegment segments) |]

-- | Quasi-quoter which can be used to generate "Query"s conveniently.
pgsq :: QuasiQuoter
pgsq =
	QuasiQuoter {
		quoteExp  = parseQuery,
		quotePat  = const (fail "Cannot use 'pgsq' in pattern"),
		quoteType = const (fail "Cannot use 'pgsq' in type"),
		quoteDec  = const (fail "Cannot use 'pgsq' in declaration")
	}
