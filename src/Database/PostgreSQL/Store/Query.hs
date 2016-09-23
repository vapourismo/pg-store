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

	-- *
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
	| QueryQuote Char String
	| QueryOther String
	deriving (Show, Eq, Ord)

-- | Segment referencing a table type
tableSegment :: Parser QuerySegment
tableSegment = do
	char '@'
	QueryTable <$> qualifiedTypeName

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
					[e| insertName (tableName (tableInfo (Proxy :: Proxy $(conT typ)))) |]

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
