{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

-- |
-- Module:     Database.PostgreSQL.Store.Query.TH
-- Copyright:  (c) Ole Krüger 2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
--
-- All of the quasi quoters in this module accept the same language. It is almost identical to the
-- language that a PostgreSQL server accepts. Certain operators have been reused in order to achieve
-- a nice integration with the features of this library.
--
-- Each quasi quoter uses 'QueryGenerator' as an intermediate representation.
--
-- = 'QueryGenerator'
-- Other query generators can be embedded using the @$(haskell code)@ construct.
--
-- > genCatSound :: QueryGenerator a
-- > genCatSound = Code "'meow'"
-- >
-- > listCats :: Query String
-- > listCats =
-- >     [pgQuery| SELECT name
-- >               FROM animals
-- >               WHERE sound = $(genCatSound) |]
--
-- We inline the @genCatSound@ generator which produces the SQL code @\'meow\'@. As a result we have
-- a query with this statement:
--
-- > SELECT *
-- > FROM animals
-- > WHERE sound = 'meow'
--
-- It is also possible to produce 'QueryGenerator's with the 'pgQueryGen' quasi quoter.
--
-- > genCatSound :: QueryGenerator a
-- > genCatSound = [pgQueryGen| 'meow' |]
--
-- = 'Entity'
-- Everything that has an instance of 'Entity' can also be used in these quasi quoters. So far, one
-- can only utilize them in the form of named expressions.
--
-- The @$@ operator will cause the value of the following named expression to be used in the query.
-- Any use of @$name@ is just a short cut for @$('embedEntity' name)@.
--
-- > listPeople :: Int -> Query String
-- > listPeople minimumAge =
-- >     [pgQuery| SELECT name
-- >               FROM people
-- >               WHERE age > $minimumAge |]
--
-- This query will list the names of people above a given age.
--
-- = 'TableEntity'
-- The 'TableEntity' type class lets you associate a table name and the name of its column with a
-- data type.
--
-- Given a table schema like the following:
--
-- > CREATE TABLE MyTable (
-- >     first  INTEGER NOT NULL,
-- >     second VARCHAR NOT NULL
-- > )
--
-- We produce Haskell code like this:
--
-- > data MyTable = MyTable {
-- >     first  :: Int,
-- >     second :: String
-- > } deriving (Show, Eq, Ord, Generic)
-- >
-- > instance Entity MyTable
-- >
-- > instance TableEntity MyTable
--
-- Alternatively we can implement 'Entity' and 'TableEntity' ourselves. In this case it is not
-- needed.
--
-- We utilize these type classes in the following way:
--
-- > listMyTable :: Query MyTable
-- > listMyTable =
-- >     [pgQuery| SELECT #MyTable
-- >               FROM @MyTable |]
--
-- We expand the absolute column names using @#MyTable@ and the table name using @\@MyTable@.
-- This results in the following SQL:
--
-- > SELECT MyTable.first, MyTable.second
-- > FROM MyTable
--
-- Aliasing the table name is also possible:
--
-- > listMyTable :: Query MyTable
-- > listMyTable =
-- >     [pgQuery| SELECT #MyTable(t)
-- >               FROM @MyTable AS t |]
--
-- The alias is included in the resulting SQL:
--
-- > SELECT t.first, t.second
-- > FROM MyTable t
--
module Database.PostgreSQL.Store.Query.TH (
	-- * Quasi quoters
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
import           Database.PostgreSQL.Store.Query.Builder

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
					[e| embedEntity $(varE name) |]

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

-- | Generate a 'QueryGenerator' expression.
--
-- See "Database.PostgreSQL.Store.Query.TH" for detailed description of the language accepted by
-- this quasi quoter.
--
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

-- | Generate a "Query". This utilizes an intermediate query generator of type @QueryGenerator ()@.
--
-- See "Database.PostgreSQL.Store.Query.TH" for detailed description of the language accepted by
-- this quasi quoter.
--
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

-- | Generate a "PrepQuery". The intermediate query generator has type @QueryGenerator (Tuple ts)@
-- where @ts@ has kind @[Type]@. @ts@ represents the types of the parameters to this prepared query.
--
-- It is highly recommended that supply a type signature, if you give the resulting expression a
-- name, to avoid ambiguity.
--
-- > q :: PrepQuery '[Int, String] User
-- > q = [pgPrepQuery| SELECT #User(u) FROM @User u WHERE age < $(param0) AND name LIKE $(param1) |]
--
-- See "Database.PostgreSQL.Store.Query.TH" for detailed description of the language accepted by
-- this quasi quoter.
--
pgPrepQuery :: QuasiQuoter
pgPrepQuery =
	QuasiQuoter {
		quoteExp  = prepQueryE,
		quotePat  = const (fail "Cannot use 'pgPrepQuery' in pattern"),
		quoteType = const (fail "Cannot use 'pgPrepQuery' in type"),
		quoteDec  = const (fail "Cannot use 'pgPrepQuery' in declaration")
	}
