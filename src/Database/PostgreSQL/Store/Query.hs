{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleInstances, UndecidableInstances #-}

-- |
-- Module:     Database.PostgreSQL.Store.Query
-- Copyright:  (c) Ole Krüger 2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.Query (
	-- * Type
	Query (..),

	-- * Template Haskell
	parseQuery,
	pgsq,
	parseQueryBuilder,
	pgsqb
) where

import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Language.Haskell.TH.Quote
import           Language.Haskell.Meta.Parse

import           Control.Applicative
import           Control.Monad.State.Strict hiding (lift)

import           Data.List
import           Data.Char
import           Data.Proxy
import           Data.Attoparsec.Text
import qualified Data.ByteString                    as B
import qualified Blaze.ByteString.Builder           as B
import qualified Blaze.ByteString.Builder.Char.Utf8 as B
import qualified Data.Text                          as T

import           Database.PostgreSQL.Store.Columns
import           Database.PostgreSQL.Store.Query.Builder

-- | Query including statement and parameters
data Query = Query {
	-- | Statement
	queryStatement :: !B.ByteString,

	-- | Parameters
	queryParams :: ![Value]
} deriving (Show, Eq, Ord)

-- | Build a "Query" using the given builder.
buildQuery :: QueryBuilder -> Query
buildQuery builder =
	Query code values
	where
		(_, code, values) = execState builder (1, B.empty, [])

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
	| QueryTableProxy String
	| QuerySelector String
	| QuerySelectorProxy String
	| QueryIdentifier String
	| QueryIdentifierProxy String
	| QueryEntity String
	| QueryEntityCode String
	| QueryQuote Char String
	| QueryOther String
	deriving (Show, Eq, Ord)

-- | Table
tableSegment :: Parser QuerySegment
tableSegment = do
	char '@'
	QueryTable <$> qualifiedTypeName

-- | Table proxy
tableProxySegment :: Parser QuerySegment
tableProxySegment = do
	char '@'
	QueryTableProxy <$> valueName

-- | Segment
selectorSegment :: Parser QuerySegment
selectorSegment = do
	char '#'
	QuerySelector <$> qualifiedTypeName

-- | Segment proxy
selectorProxySegment :: Parser QuerySegment
selectorProxySegment = do
	char '#'
	QuerySelectorProxy <$> valueName

-- | Identifier
identifierSegment :: Parser QuerySegment
identifierSegment = do
	char '&'
	QueryIdentifier <$> qualifiedTypeName

-- | Identifier proxy
identifierProxySegment :: Parser QuerySegment
identifierProxySegment = do
	char '&'
	QueryIdentifierProxy <$> valueName

-- |
entityCodeSegment :: Parser QuerySegment
entityCodeSegment =
	QueryEntityCode <$> (string "${" *> insideCode <* char '}')
	where
		insideCode =
			concat <$> many (choice [bracedCode,
			                         quoteCode '\'',
			                         quoteCode '\"',
			                         some (satisfy (notInClass "\"'{}"))])

		bracedCode =
			char '{' *> insideCode <* char '}'

		quoteCode delim = do
			char delim
			cnt <- many (choice [escapedDelim delim, notDelim delim])
			char delim
			pure (concat cnt)

		escapedDelim delim = do
			char '\\'
			char delim
			pure ['\\', delim]

		notDelim delim =
			(: []) <$> notChar delim

-- | Entity
entityNameSegment :: Parser QuerySegment
entityNameSegment = do
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
	        tableProxySegment,
	        selectorSegment,
	        selectorProxySegment,
	        identifierSegment,
	        identifierProxySegment,
	        entityCodeSegment,
	        entityNameSegment,
	        otherSegment]

-- | Lift "ByteString".
liftByteString :: B.ByteString -> Q Exp
liftByteString bs =
	[e| B.pack $(lift (B.unpack bs)) |]

-- | Pack 'String' code into a 'ByteString'.
packCode :: String -> B.ByteString
packCode code =
	B.toByteString (B.fromString code)

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

		QueryTableProxy valueName -> do
			mbTypeName <- lookupValueName valueName
			case mbTypeName of
				Nothing -> fail ("'" ++ valueName ++ "' does not refer to a value")
				Just name ->
					[e| insertTableName ((const Proxy :: a -> Proxy a) $(varE name)) |]

		QuerySelector typeName -> do
			mbTypeName <- lookupTypeName typeName
			case mbTypeName of
				Nothing -> fail ("'" ++ typeName ++ "' does not refer to a type")
				Just typ ->
					[e| insertTableColumnNames (Proxy :: Proxy $(conT typ)) |]

		QuerySelectorProxy valueName -> do
			mbTypeName <- lookupValueName valueName
			case mbTypeName of
				Nothing -> fail ("'" ++ valueName ++ "' does not refer to a value")
				Just name ->
					[e| insertTableColumnNames ((const Proxy :: a -> Proxy a) $(varE name)) |]

		QueryIdentifier typeName -> do
			mbTypeName <- lookupTypeName typeName
			case mbTypeName of
				Nothing -> fail ("'" ++ typeName ++ "' does not refer to a type")
				Just typ ->
					[e| insertTableIdentColumnName (Proxy :: Proxy $(conT typ)) |]

		QueryIdentifierProxy valueName -> do
			mbTypeName <- lookupValueName valueName
			case mbTypeName of
				Nothing -> fail ("'" ++ valueName ++ "' does not refer to a value")
				Just name ->
					[e| insertTableIdentColumnName ((const Proxy :: a -> Proxy a) $(varE name)) |]

		QueryEntity valueName -> do
			mbValueName <- lookupValueName valueName
			case mbValueName of
				Nothing -> fail ("'" ++ valueName ++ "' does not refer to a value")
				Just name ->
					[e| insertEntity $(varE name) |]

		QueryEntityCode code ->
			case parseExp code of
				Left msg -> fail ("Error in code " ++ show code ++ ": " ++ msg)
				Right exp ->
					[e| insertEntity $(pure exp) |]

		QueryQuote _ code ->
			[e| insertCode $(liftByteString (packCode code)) |]

		QueryOther code ->
			[e| insertCode $(liftByteString (packCode code)) |]

-- | Parse a query string.
parseQuery :: String -> Q Exp
parseQuery code =
	case parseOnly (many querySegment <* endOfInput) (T.strip (T.pack code)) of
		Left msg ->
			fail ("Query parser failed: " ++ msg)

		Right [] ->
			[e| Query B.empty [] |]

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

-- | Parse a query string.
parseQueryBuilder :: String -> Q Exp
parseQueryBuilder code =
	case parseOnly (many querySegment <* endOfInput) (T.strip (T.pack code)) of
		Left msg ->
			fail ("Query parser failed: " ++ msg)

		Right [] ->
			[e| pure () |]

		Right segments ->
			[e| $(DoE . map NoBindS <$> mapM translateSegment segments) |]

-- | Quasi-quoter which can be used to generate "QueryBuilder"s conveniently.
pgsqb :: QuasiQuoter
pgsqb =
	QuasiQuoter {
		quoteExp  = parseQueryBuilder,
		quotePat  = const (fail "Cannot use 'pgsqb' in pattern"),
		quoteType = const (fail "Cannot use 'pgsqb' in type"),
		quoteDec  = const (fail "Cannot use 'pgsqb' in declaration")
	}
