{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

-- |
-- Module:     Database.PostgreSQL.Store.Query.TH
-- Copyright:  (c) Ole Krüger 2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.Query.TH (
	-- * Template Haskell
	parseQuery,
	pgsq
) where

import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Language.Haskell.Meta.Parse

import           Control.Applicative

-- import           Data.List
-- import           Data.Proxy
import           Data.Char
import           Data.Attoparsec.Text
import qualified Data.ByteString                    as B
import qualified Blaze.ByteString.Builder           as B
import qualified Blaze.ByteString.Builder.Char.Utf8 as B
import qualified Data.Text                          as T

import           Database.PostgreSQL.Store.Utilities
import           Database.PostgreSQL.Store.Query.Builder
import           Database.PostgreSQL.Store.Query.Entity

-- | Name
valueName :: Parser String
valueName =
	(:) <$> (letter <|> char '_') <*> many (satisfy isAlphaNum <|> char '_' <|> char '\'')

-- -- | Type name
-- typeName :: Parser String
-- typeName =
-- 	(:) <$> satisfy isUpper <*> many (satisfy isAlphaNum <|> char '_' <|> char '\'')

-- -- | Qualified type name
-- qualifiedTypeName :: Parser String
-- qualifiedTypeName =
-- 	intercalate "." <$> sepBy1 typeName (char '.')

-- | Query segment
data QuerySegment
	= QueryEntity String
	| QueryEntityCode String
	| QueryQuote Char String
	| QueryOther String
	-- QueryTable String
	-- QueryTableProxy String
	-- QuerySelector String
	-- QuerySelectorProxy String
	-- QueryIdentifier String
	-- QueryIdentifierProxy String
	deriving (Show, Eq, Ord)

-- -- | Table
-- tableSegment :: Parser QuerySegment
-- tableSegment = do
-- 	char '@'
-- 	QueryTable <$> qualifiedTypeName

-- -- | Table proxy
-- tableProxySegment :: Parser QuerySegment
-- tableProxySegment = do
-- 	char '@'
-- 	QueryTableProxy <$> valueName

-- -- | Segment
-- selectorSegment :: Parser QuerySegment
-- selectorSegment = do
-- 	char '#'
-- 	QuerySelector <$> qualifiedTypeName

-- -- | Segment proxy
-- selectorProxySegment :: Parser QuerySegment
-- selectorProxySegment = do
-- 	char '#'
-- 	QuerySelectorProxy <$> valueName

-- -- | Identifier
-- identifierSegment :: Parser QuerySegment
-- identifierSegment = do
-- 	char '&'
-- 	QueryIdentifier <$> qualifiedTypeName

-- -- | Identifier proxy
-- identifierProxySegment :: Parser QuerySegment
-- identifierProxySegment = do
-- 	char '&'
-- 	QueryIdentifierProxy <$> valueName

-- | Entity
entityNameSegment :: Parser QuerySegment
entityNameSegment = do
	char '$'
	QueryEntity <$> valueName

-- | Entity code
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
			char '{' *> fmap (\ code -> '{' : code ++ "}") insideCode <* char '}'

		quoteCode delim = do
			char delim
			cnt <- many (choice [escapedDelim delim, notDelim delim])
			char delim
			pure (delim : concat cnt ++ [delim])

		escapedDelim delim = do
			char '\\'
			char delim
			pure ['\\', delim]

		notDelim delim =
			(: []) <$> notChar delim

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
	QueryOther <$> some (satisfy (notInClass "\"'$"))
	-- QueryOther <$> some (satisfy (notInClass "\"'@&#$"))

-- | Segment that is part of the query
querySegment :: Parser QuerySegment
querySegment =
	choice [quoteSegment '\'',
	        quoteSegment '"',
	        -- tableSegment,
	        -- tableProxySegment,
	        -- selectorSegment,
	        -- selectorProxySegment,
	        -- identifierSegment,
	        -- identifierProxySegment,
	        entityCodeSegment,
	        entityNameSegment,
	        otherSegment]

-- | Pack 'String' code into a 'ByteString'.
packCode :: String -> B.ByteString
packCode code =
	B.toByteString (B.fromString code)

-- | Translate a "QuerySegment" to an expression.
translateSegment :: QuerySegment -> Q Exp
translateSegment segment =
	case segment of
		-- QueryTable stringName -> do
		-- 	mbTypeName <- lookupTypeName stringName
		-- 	case mbTypeName of
		-- 		Nothing -> fail ("'" ++ stringName ++ "' does not refer to a type")
		-- 		Just typ ->
		-- 			[e| insertTableName (Proxy :: Proxy $(conT typ)) |]

		-- QueryTableProxy stringName -> do
		-- 	mbTypeName <- lookupValueName stringName
		-- 	case mbTypeName of
		-- 		Nothing -> fail ("'" ++ stringName ++ "' does not refer to a value")
		-- 		Just name ->
		-- 			[e| insertTableName ((const Proxy :: a -> Proxy a) $(varE name)) |]

		-- QuerySelector stringName -> do
		-- 	mbTypeName <- lookupTypeName stringName
		-- 	case mbTypeName of
		-- 		Nothing -> fail ("'" ++ stringName ++ "' does not refer to a type")
		-- 		Just typ ->
		-- 			[e| insertTableColumnNames (Proxy :: Proxy $(conT typ)) |]

		-- QuerySelectorProxy stringName -> do
		-- 	mbTypeName <- lookupValueName stringName
		-- 	case mbTypeName of
		-- 		Nothing -> fail ("'" ++ stringName ++ "' does not refer to a value")
		-- 		Just name ->
		-- 			[e| insertTableColumnNames ((const Proxy :: a -> Proxy a) $(varE name)) |]

		-- QueryIdentifier stringName -> do
		-- 	mbTypeName <- lookupTypeName stringName
		-- 	case mbTypeName of
		-- 		Nothing -> fail ("'" ++ stringName ++ "' does not refer to a type")
		-- 		Just typ ->
		-- 			[e| insertTableIdentColumnName (Proxy :: Proxy $(conT typ)) |]

		-- QueryIdentifierProxy stringName -> do
		-- 	mbTypeName <- lookupValueName stringName
		-- 	case mbTypeName of
		-- 		Nothing -> fail ("'" ++ stringName ++ "' does not refer to a value")
		-- 		Just name ->
		-- 			[e| insertTableIdentColumnName ((const Proxy :: a -> Proxy a) $(varE name)) |]

		QueryEntity stringName -> do
			mbValueName <- lookupValueName stringName
			case mbValueName of
				Nothing -> fail ("'" ++ stringName ++ "' does not refer to a value")
				Just name ->
					[e| insertEntity $(varE name) |]

		QueryEntityCode code ->
			case parseExp code of
				Left msg -> fail ("Error in code " ++ show code ++ ": " ++ msg)
				Right expr ->
					[e| insertEntity $(pure expr) |]

		QueryQuote delim code ->
			[e| insertCode $(liftByteString (packCode (delim : code ++ [delim]))) |]

		QueryOther code ->
			[e| insertCode $(liftByteString (packCode code)) |]

-- | Parse a query string in order to produce a 'QueryBuilder' expression.
parseQuery :: String -> Q Exp
parseQuery code =
	case parseOnly (many querySegment <* endOfInput) (T.strip (T.pack code)) of
		Left msg ->
			fail ("Query parser failed: " ++ msg)

		Right [] ->
			[e| buildQuery (pure ()) |]

		Right segments ->
			[e| buildQuery $(DoE . map NoBindS <$> mapM translateSegment segments) |]

-- | Generate queries conveniently. See 'BuildQuery' to find out which types can be produced.
pgsq :: QuasiQuoter
pgsq =
	QuasiQuoter {
		quoteExp  = parseQuery,
		quotePat  = const (fail "Cannot use 'pgsq' in pattern"),
		quoteType = const (fail "Cannot use 'pgsq' in type"),
		quoteDec  = const (fail "Cannot use 'pgsq' in declaration")
	}