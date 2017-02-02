{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Database.PostgreSQL.Store.Query.TH2 (
	-- * Template Haskell
	pgQueryGen,
	pgQuery,
	pgPrepQuery
) where

import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Language.Haskell.Meta.Parse

import           Control.Applicative

import           Data.Tagged
import           Data.List
import           Data.Char
import           Data.Attoparsec.Text
import qualified Data.ByteString                    as B
import qualified Blaze.ByteString.Builder           as B
import qualified Blaze.ByteString.Builder.Char.Utf8 as B
import qualified Data.Text                          as T

import           Database.PostgreSQL.Store.Utilities
import           Database.PostgreSQL.Store.Entity
import           Database.PostgreSQL.Store.Table
import           Database.PostgreSQL.Store.Query.Builder2

-- | Name
valueName :: Parser String
valueName =
	(:) <$> (letter <|> char '_') <*> many (satisfy isAlphaNum <|> char '_' <|> char '\'')

-- | Type name
typeName :: Parser String
typeName =
	(:) <$> satisfy isUpper <*> many (satisfy isAlphaNum <|> char '_' <|> char '\'')

-- | Qualified type name
qualifiedTypeName :: Parser String
qualifiedTypeName =
	intercalate "." <$> sepBy1 typeName (char '.')

-- | Query segment
data QuerySegment
	= QueryEntity String
	| QueryEntityCode String
	| QueryQuote Char String
	| QueryOther String
	| QueryTable String
	| QuerySelector String
	| QuerySelectorAlias String String
	deriving (Show, Eq, Ord)

-- | Table
tableSegment :: Parser QuerySegment
tableSegment = do
	char '@'
	QueryTable <$> qualifiedTypeName

-- | Selector
selectorSegment :: Parser QuerySegment
selectorSegment = do
	char '#'
	QuerySelector <$> qualifiedTypeName

-- | Selector alias
selectorAliasSegment :: Parser QuerySegment
selectorAliasSegment = do
	char '#'
	QuerySelectorAlias <$> qualifiedTypeName
	                   <*  char '('
	                   <*> valueName
	                   <*  char ')'

-- | Entity
entityNameSegment :: Parser QuerySegment
entityNameSegment = do
	char '$'
	QueryEntity <$> valueName

-- | Entity code
entityCodeSegment :: Parser QuerySegment
entityCodeSegment =
	QueryEntityCode <$> (string "$(" *> insideCode <* char ')')
	where
		insideCode =
			concat <$> many (choice [bracedCode,
			                         quoteCode '\'',
			                         quoteCode '\"',
			                         some (satisfy (notInClass "\"'()"))])

		bracedCode =
			char '(' *> fmap (\ code -> '(' : code ++ ")") insideCode <* char ')'

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
	QueryOther <$> some (satisfy (notInClass "\"'@#$"))

-- | Segment that is part of the query
querySegment :: Parser QuerySegment
querySegment =
	choice [quoteSegment '\'',
	        quoteSegment '"',
	        tableSegment,
	        selectorAliasSegment,
	        selectorSegment,
	        entityCodeSegment,
	        entityNameSegment,
	        otherSegment]

-- | Pack 'String' code into a 'ByteString'.
packCode :: String -> B.ByteString
packCode code =
	B.toByteString (B.fromString code)

-- | Shortcut for @Tagged t Table@
type TableTag t = Tagged t Table

-- |
tableDescriptionE :: Name -> Q Exp
tableDescriptionE typ =
	[e| untag (describeTableType :: TableTag $(conT typ)) |]

-- | Translate a "QuerySegment" to an expression.
translateSegment :: QuerySegment -> Q Exp
translateSegment segment =
	case segment of
		QueryTable stringName -> do
			mbTypeName <- lookupTypeName stringName
			case mbTypeName of
				Nothing ->
					fail ("'" ++ stringName ++ "' does not refer to a type")
				Just typ ->
					[e| genTableName $(tableDescriptionE typ) |]

		QuerySelector stringName -> do
			mbTypeName <- lookupTypeName stringName
			case mbTypeName of
				Nothing ->
					fail ("'" ++ stringName ++ "' does not refer to a type")
				Just typ ->
					[e| genTableColumns $(tableDescriptionE typ) |]

		QuerySelectorAlias stringName aliasName -> do
			mbTypeName <- lookupTypeName stringName
			case mbTypeName of
				Nothing ->
					fail ("'" ++ stringName ++ "' does not refer to a type")
				Just typ ->
					[e| genTableColumnsOn $(tableDescriptionE typ)
					                      $(liftByteString (buildByteString aliasName)) |]

		QueryEntity stringName -> do
			mbValueName <- lookupValueName stringName
			case mbValueName of
				Nothing ->
					fail ("'" ++ stringName ++ "' does not refer to a value")
				Just name ->
					[e| genEntity $(varE name) |]

		QueryEntityCode code ->
			case parseExp code of
				Left msg   -> fail ("Error in code " ++ show code ++ ": " ++ msg)
				Right expr -> pure expr

		QueryQuote delim code ->
			[e| Code $(liftByteString (packCode (delim : code ++ [delim]))) |]

		QueryOther code ->
			[e| Code $(liftByteString (packCode code)) |]

-- | Parse a query string in order to produce a 'QueryGenerator' expression.
queryGenE :: String -> Q Exp
queryGenE code =
	case parseOnly (many querySegment <* endOfInput) (T.strip (T.pack code)) of
		Left msg ->
			fail ("Query parser failed: " ++ msg)

		Right [] ->
			[e| mempty |]

		Right segments ->
			[e| mconcat $(ListE <$> mapM translateSegment segments) |]

-- |
pgQueryGen :: QuasiQuoter
pgQueryGen =
	QuasiQuoter {
		quoteExp  = queryGenE,
		quotePat  = const (fail "Cannot use 'pgQueryGen' in pattern"),
		quoteType = const (fail "Cannot use 'pgQueryGen' in type"),
		quoteDec  = const (fail "Cannot use 'pgQueryGen' in declaration")
	}

-- |
queryE :: String -> Q Exp
queryE code =
	[e| assemble $(queryGenE code) () |]

-- | Generate queries conveniently.
pgQuery :: QuasiQuoter
pgQuery =
	QuasiQuoter {
		quoteExp  = queryE,
		quotePat  = const (fail "Cannot use 'pgQuery' in pattern"),
		quoteType = const (fail "Cannot use 'pgQuery' in type"),
		quoteDec  = const (fail "Cannot use 'pgQuery' in declaration")
	}

-- |
prepQueryE :: String -> Q Exp
prepQueryE code = do
	Loc _ p m _ _ <- location
	withPrefix (B.concat [buildByteString p, "_", buildByteString m, "_"])
	where
		withPrefix prefix =
			[e| assemblePrep $(liftByteString prefix) $(queryGenE code) |]

-- |
pgPrepQuery :: QuasiQuoter
pgPrepQuery =
	QuasiQuoter {
		quoteExp  = prepQueryE,
		quotePat  = const (fail "Cannot use 'pgPrepQuery' in pattern"),
		quoteType = const (fail "Cannot use 'pgPrepQuery' in type"),
		quoteDec  = const (fail "Cannot use 'pgPrepQuery' in declaration")
	}
