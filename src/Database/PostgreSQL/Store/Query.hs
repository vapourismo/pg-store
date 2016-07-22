{-# LANGUAGE OverloadedStrings, TemplateHaskell, BangPatterns, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

-- |
-- Module:     Database.PostgreSQL.Store.Query
-- Copyright:  (c) Ole Krüger 2015-2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.Query (
	-- * Query
	Query (..),
	SelectorElement (..),
	QueryTable (..),
	pgsq,
	pgss,

	-- * Helpers
	quoteIdentifier,
	makeTableIdentifier,
	makeTableSelectors,

	-- * Query builder
	QueryBuild (..),
	QueryBuilder,
	runQueryBuilder,
	writeParam,
	writeCode,
	writeStringCode,
	writeIdentifier,
	writeAbsIdentifier,
	intercalateBuilder
) where

import           Language.Haskell.TH
import           Language.Haskell.TH.Quote

import           Control.Applicative
import           Control.Monad.State.Strict

import           Data.List
import           Data.Proxy
import           Data.String
import qualified Data.Text          as T
import qualified Data.ByteString    as B

import           Data.Char
import           Data.Attoparsec.Text

import           Database.PostgreSQL.Store.Columns

-- | Properly quote an identifier.
quoteIdentifier :: String -> String
quoteIdentifier name =
	'"' : stripQuotes name ++ "\""
	where
		stripQuotes []         = []
		stripQuotes ('"' : xs) = '"' : '"' : stripQuotes xs
		stripQuotes (x : xs)   = x : stripQuotes xs

-- | Query including statement and parameters
--
-- Use the 'pgsq' quasi-quoter to conveniently create queries.
data Query = Query {
	-- | Statement
	queryStatement :: !B.ByteString,

	-- | Parameters
	queryParams :: ![Value]
} deriving (Show, Eq, Ord)

-- | @SELECT@ expression
data SelectorElement
	= SelectorField String
	  -- ^ Select a field. The field nme will be quoted and properly escaped.
	| SelectorSpecial String
	  -- ^ Select a special expression. The expression will be inlined as is.
	deriving (Show, Eq, Ord)

-- | A type which implements this class can be used as a table in a quasi-quoted query.
--   'mkTable' can implement this for you.
class QueryTable a where
	-- | Unquoted name of the table
	tableName :: Proxy a -> String

	-- | Unquoted name of the ID field
	tableIDName :: Proxy a -> String

	-- | Selectors needed to retrieve all fields necessary to construct the type - think @SELECT@.
	tableSelectors :: Proxy a -> [SelectorElement]

-- | Generate the quoted identifier for a table.
makeTableIdentifier :: (QueryTable a) => Proxy a -> String
makeTableIdentifier proxy =
	quoteIdentifier (tableName proxy)

-- | Generate table name expression.
tableNameE :: Name -> Q Exp
tableNameE typName =
	[e| makeTableIdentifier (Proxy :: Proxy $(conT typName)) |]

-- | Generate table ID name expression
tableIDNameE :: Name -> Q Exp
tableIDNameE typName =
	[e| quoteIdentifier (tableIDName (Proxy :: Proxy $(conT typName))) |]

-- | Generate absolute table ID name expression.
tableAbsoluteIDNameE :: Name -> Q Exp
tableAbsoluteIDNameE typName =
	[e| $(tableNameE typName) ++ "." ++ $(tableIDNameE typName) |]

-- | Generate the list of expression used as selector.
makeTableSelectors :: (QueryTable a) => Proxy a -> String
makeTableSelectors proxy =
	intercalate ", " (map makeElement (tableSelectors proxy))
	where
		makeElement (SelectorField name) =
			quoteIdentifier (tableName proxy) ++ "." ++ quoteIdentifier name
		makeElement (SelectorSpecial expr) =
			expr

-- | Generate the selector list expression.
makeTableSelectorsE :: Name -> Q Exp
makeTableSelectorsE typName =
	[e| makeTableSelectors (Proxy :: Proxy $(conT typName)) |]

-- | Query segment
data Segment
	= SSelector String
	| STable String
	| SVariable String
	| SIdentifier String
	| SQuote Char String
	| SOther String

-- | Name
name :: Parser String
name =
	(:) <$> (letter <|> char '_') <*> many (satisfy isAlphaNum <|> char '_')

-- | Type name
typeName :: Parser String
typeName =
	(:) <$> satisfy isUpper <*> many (satisfy isAlphaNum <|> char '_')

-- | Qualified type name
qualifiedTypeName :: Parser String
qualifiedTypeName = do
	intercalate "." <$> sepBy1 typeName (char '.')

-- | Quote
quote :: Char -> Parser Segment
quote delim = do
	char delim
	cnt <- concat <$> many (choice [escapedDelim, notDelim])
	char delim
	pure (SQuote delim cnt)
	where
		escapedDelim = (\ a b -> [a, b]) <$> char '\\' <*> char delim
		notDelim = (: []) <$> notChar delim

-- | Segments
segments :: Parser [Segment]
segments =
	many (choice [quote '"',
	              quote '\'',
	              char '#' >> SSelector <$> qualifiedTypeName,
	              char '@' >> STable <$> qualifiedTypeName,
	              char '&' >> SIdentifier <$> qualifiedTypeName,
	              char '$' >> SVariable <$> name,
	              SOther "#" <$ char '#',
	              SOther "@" <$ char '@',
	              SOther "&" <$ char '&',
	              SOther "$" <$ char '$',
	              SOther <$> some (satisfy (notInClass "\"'#@&$"))])

-- | Reduce segments in order to resolve names and collect query parameters.
reduceSegment :: Segment -> QueryBuilder [Q Exp] (Q Exp)
reduceSegment seg =
	case seg of
		SOther str ->
			writeCode [stringE str]

		SQuote delim cnt ->
			writeCode [stringE (delim : cnt ++ [delim])]

		SVariable varName ->
			writeParam $ do
				mbName <- lookupValueName varName
				maybe (fail ("'" ++ varName ++ "' does not refer to anything"))
				      (\ name -> [e| pack $(varE name) |])
				      mbName

		STable tableName ->
			writeCode [do mbName <- lookupTypeName tableName
			              maybe (fail ("'" ++ tableName ++ "' does not refer to anything"))
			                    tableNameE -- Replace with table name
			                    mbName]

		SSelector tableName ->
			writeCode [do mbName <- lookupTypeName tableName
			              maybe (fail ("'" ++ tableName ++ "' does not refer to anything"))
			                    makeTableSelectorsE -- Replace with selector list
			                    mbName]

		SIdentifier tableName ->
			writeCode [do mbName <- lookupTypeName tableName
			              maybe (fail ("'" ++ tableName ++ "' does not refer to anything"))
			                    tableAbsoluteIDNameE -- Replace with absolute ID name
			                    mbName]

-- | Parse quasi-quoted query.
parseStoreQueryE :: String -> Q Exp
parseStoreQueryE code = do
	case parseOnly (segments <* endOfInput) (T.pack code) of
		Left msg ->
			fail msg

		Right xs -> do
			runQueryBuilder (mapM_ reduceSegment xs)

-- | This quasi-quoter allows you to generate instances of 'Query'. It lets you write SQL with some
-- small enhancements. 'pgsq' heavily relies on 'QueryTable' which can be implemented by 'mkTable'
-- for a type of your choice.
--
-- Some syntax definitions that might be useful later on:
--
-- > TypeName          ::= UpperAlpha {AlphaNumeric | '_'}
-- > Name              ::= (Alpha | '_') {AlphaNumeric | '_'}
-- > QualifiedTypeName ::= {TypeName '.'} TypeName
--
-- @Alpha@ includes all alphabetical characters; @UpperAlpha@ includes all upper-case alphabetical
-- characters; @AlphaNumeric@ includes all alpha-numeric characters.
--
-- = Embed values
-- You can embed values whose types implement 'Column'.
--
-- > ValueExp ::= '$' Name
--
-- > magicNumber :: Int
-- > magicNumber = 1337
-- >
-- > myQuery :: Query
-- > myQuery =
-- >     [pgsq| SELECT * FROM table t WHERE t.column1 > $magicNumber AND t.column2 < $otherNumber |]
-- >     where otherNumber = magicNumber * 2
--
-- @$magicNumber@ and @$otherNumber@ are references to values @magicNumber@ and @otherNumber@.
--
-- The quasi-quoter will generate a 'Query' expression similar to the following.
--
-- > Query "SELECT * FROM table t WHERE t.column1 > $1 AND t.column2 < $2"
-- >       [pack magicNumber, pack otherNumber]
--
-- = Table names
-- Types that implement 'QueryTable' associate a table name with themselves. Since the table name is
-- not always known to the user, one can insert it dynamically.
--
-- > TableNameExp ::= '@' QualifiedTypeName
--
-- The @\@@-operators is also an alias for the function @ABS@. If you have an expression that
-- triggers the quasi-quoter such as @\@A@, but you would like to use the @ABS@ functionality, then
-- simply reformat your expression to @\@(A)@ or @\@\"A\"@ or @ABS(A)@.
--
-- > instance QueryTable YourType where
-- >     tableName _ = "YourTable"
-- >
-- > myQuery :: Query
-- > myQuery =
-- >     [pgsq| SELECT * FROM @Table WHERE @Table.column = 1337 |]
--
-- The table name will be inlined which results in the following.
--
-- > Query "SELECT * FROM \"YourTable\" WHERE \"YourTable\".column = 1337" []
--
-- = Identifier column names
-- Each instance of 'QueryTable' also provides the name of the identifier column. Using this column
-- name you can identify specific rows of a certain table.
--
-- > TableIdentExp ::= '&' TypeName
--
-- @&@ is also the operator for bitwise-AND. To resolve the ambiguity for expressions like @A&B@,
-- simply reformat it to @A & B@ or @A&(B)@ or @A&\"B\"@.
--
-- > instance QueryTable YourType where
-- >     tableName _   = "YourTable"
-- >     tableIDName _ = "id"
-- >
-- > listIDs :: Query
-- > listIDs =
-- >     [pgsq| SELECT &YourType FROM @YourType |]
--
-- @listIDs@ is now a query which lists the IDs of each row. This is especially useful in
-- combination with 'Reference'.
--
-- > fetchIDs :: Errand [Reference YourType]
-- > fetchIDs =
-- >     query [pgsq| SELECT &YourType FROM @YourType |]
--
-- Note, just like @\@@, the operator @&@ is reserved. Although @A&B@ is a valid SQL expression, it
-- will trigger the quasi-quoter. To avoid this, you reform your expression to @A & B@ or @A&(B)@.
--
-- = Selectors
-- 'mkTable' will automatically implement 'Result' and 'QueryTable' for you. This allows you to make
-- use of the selector expander.
--
-- > SelectorExp ::= '#' QualifiedTypeName
--
-- @#@ is also the operator for bitwise-XOR. To resolve the ambiguity for expressions like @A#B@,
-- simply reformat it to @A # B@ or @A#(B)@ or @A#\"B\"@.
--
-- > data Actor = Actor {
-- >     actorName :: String,
-- >     actorAge  :: Word
-- > } deriving (Show, Eq, Ord)
-- >
-- > mkTable ''Actor []
-- >
-- > fetchOldActors :: Errand [Actor]
-- > fetchOldActors =
-- >     query [pgsq| SELECT #Actor FROM @Actor a WHERE a.actorAge >= $oldAge |]
-- >     where oldAge = 70
--
-- @#Actor@ will expand to a list of columns that are necessary to construct an instance of @Actor@.
-- In this case it is equivalent to
--
-- > @Actor.actorName, @Actor.actorAge
pgsq :: QuasiQuoter
pgsq =
	QuasiQuoter {
		quoteExp  = parseStoreQueryE,
		quotePat  = const (fail "Cannot use 'pgsq' in pattern"),
		quoteType = const (fail "Cannot use 'pgsq' in type"),
		quoteDec  = const (fail "Cannot use 'pgsq' in declaration")
	}

-- | Parse quasi-quoted query but return only statement.
parseStoreStatementE :: String -> Q Exp
parseStoreStatementE code = do
	case parseOnly (segments <* endOfInput) (T.pack code) of
		Left msg ->
			fail (show msg)

		Right xs -> do
			[e| fromString (concat $(listE (runQueryBuilder_ (mapM_ reduceSegment xs)))) |]

-- | Just like 'pgsq' but only produces the statement associated with the query. Referenced
-- values are not inlined, they are simply dismissed.
pgss :: QuasiQuoter
pgss =
	QuasiQuoter {
		quoteExp  = parseStoreStatementE,
		quotePat  = const (fail "Cannot use 'pgss' in pattern"),
		quoteType = const (fail "Cannot use 'pgss' in type"),
		quoteDec  = const (fail "Cannot use 'pgss' in declaration")
	}

-- | Build 'r' using 's' and 'v'.
class (Monoid s) => QueryBuild s v r | s v -> r where
	buildQuery :: s -> [v] -> r

instance QueryBuild B.ByteString Value Query where
	buildQuery = Query

instance QueryBuild String Value Query where
	buildQuery statement = Query (fromString statement)

instance QueryBuild String (Q Exp) (Q Exp) where
	buildQuery statement values =
		[e| Query (fromString $(stringE statement)) $(listE values) |]

instance QueryBuild [Q Exp] (Q Exp) (Q Exp) where
	buildQuery statements values =
		[e| Query (fromString (concat $(listE statements))) $(listE values) |]

class QueryCode s where
	makeCode :: String -> s

instance QueryCode B.ByteString where
	makeCode = fromString

instance QueryCode String where
	makeCode = id

instance QueryCode [Q Exp] where
	makeCode str = [stringE str]

-- | Internal state for 'QueryBuilder'
data QueryBuilderState s v = QueryBuilderState s Word [v]

-- | Query builder
type QueryBuilder s v = State (QueryBuilderState s v) ()

-- | Execute the query builder.
runQueryBuilder :: (QueryBuild s v r) => QueryBuilder s v -> r
runQueryBuilder builder =
	buildQuery statement values
	where
		QueryBuilderState statement _ values =
			execState builder (QueryBuilderState mempty 1 [])

-- | Execute query builder, but only retrieve the statement.
runQueryBuilder_ :: (Monoid s) => QueryBuilder s v -> s
runQueryBuilder_ builder =
	statement
	where
		QueryBuilderState statement _ _ =
			execState builder (QueryBuilderState mempty 1 [])

-- | Write a piece of code.
writeCode :: (Monoid s) => s -> QueryBuilder s v
writeCode code =
	modify $ \ (QueryBuilderState statement index values) ->
		QueryBuilderState (mappend statement code) index values

-- | Write a piece of code.
writeStringCode :: (QueryCode s, Monoid s) => String -> QueryBuilder s v
writeStringCode code =
	writeCode (makeCode code)

-- | Embed a value into the query.
writeParam :: (Monoid s, QueryCode s) => v -> QueryBuilder s v
writeParam value = do
	modify $ \ (QueryBuilderState statement index values) ->
		QueryBuilderState (mappend statement (makeCode ("$" ++ show index)))
		                  (index + 1)
		                  (values ++ [value])

-- | Insert an identifier into the query.
writeIdentifier :: (Monoid s, QueryCode s) => String -> QueryBuilder s v
writeIdentifier name =
	writeStringCode (quoteIdentifier name)

-- |  Insert an absolute identifier into the query.
writeAbsIdentifier :: (Monoid s, QueryCode s) => String -> String -> QueryBuilder s v
writeAbsIdentifier table field = do
	writeIdentifier table
	writeStringCode "."
	writeIdentifier field

-- | Insert a query builder between other builders.
intercalateBuilder :: QueryBuilder s v -> [QueryBuilder s v] -> QueryBuilder s v
intercalateBuilder x xs =
	sequence_ (intersperse x xs)
