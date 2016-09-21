{-# LANGUAGE QuasiQuotes,
             TemplateHaskell,
             BangPatterns,
             MultiParamTypeClasses,
             FunctionalDependencies,
             FlexibleInstances,
             FlexibleContexts,
             TypeFamilies #-}

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

	-- * Query builder
	QueryCode,
	QueryBuildable,
	QueryBuilder,
	runQueryBuilder,
	writeCode,
	writeStringCode,
	writeIdentifier,
	writeAbsIdentifier,
	writeParam,
	writeColumn,
	intercalateBuilder
) where

import           Language.Haskell.TH
import           Language.Haskell.TH.Quote

import           Control.Applicative
import           Control.Monad.State.Strict

import           Data.List
import           Data.Proxy
import           Data.Monoid
import           Data.String
import qualified Data.Text       as T
import qualified Data.ByteString as B

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
makeTableIdentifier :: (QueryTable a, IsString b) => Proxy a -> b
makeTableIdentifier proxy =
	fromString (quoteIdentifier (tableName proxy))

-- | Generate the quoted identifier for table's id column.
makeTableIDIdentifier :: (QueryTable a, IsString b) => Proxy a -> b
makeTableIDIdentifier proxy =
	fromString (quoteIdentifier (tableName proxy) ++
	            "." ++
	            quoteIdentifier (tableIDName proxy))

-- | Generate the list of expression used as selector.
makeTableSelectors :: (QueryTable a, IsString b) => Proxy a -> b
makeTableSelectors proxy =
	fromString (intercalate ", " (map makeElement (tableSelectors proxy)))
	where
		makeElement (SelectorField name) =
			quoteIdentifier (tableName proxy) ++ "." ++ quoteIdentifier name
		makeElement (SelectorSpecial expr) =
			expr

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
			writeStringCode str

		SQuote delim cnt ->
			writeStringCode (delim : cnt ++ [delim])

		SVariable varName -> writeParam $ do
			mbName <- lookupValueName varName
			case mbName of
				Just name -> [e| pack $(varE name) |]
				Nothing   -> fail ("'" ++ varName ++ "' does not refer to anything")

		STable tableName -> writeCode $ do
			mbName <- lookupTypeName tableName
			case mbName of
				Just table -> [e| makeTableIdentifier (Proxy :: Proxy $(conT table)) |]
				Nothing    -> fail ("'" ++ tableName ++ "' does not refer to anything")

		SSelector tableName -> writeCode $ do
			mbName <- lookupTypeName tableName
			case mbName of
				Just table -> [e| makeTableSelectors (Proxy :: Proxy $(conT table)) |]
				Nothing    -> fail ("'" ++ tableName ++ "' does not refer to anything")

		SIdentifier tableName -> writeCode $ do
			mbName <- lookupTypeName tableName
			case mbName of
				Just table -> [e| makeTableIDIdentifier (Proxy :: Proxy $(conT table)) |]
				Nothing    -> fail ("'" ++ tableName ++ "' does not refer to anything")

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
-- simply reformat your expression to @\@(A)@ or @ABS(A)@.
--
-- > instance QueryTable YourType where
-- >     tableName _ = "YourTable"
-- >
-- > myQuery :: Query
-- > myQuery =
-- >     [pgsq| SELECT * FROM @YourType WHERE @YourType.column = 1337 |]
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
-- simply reformat it to @A & B@ or @A&(B)@.
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
			[e| mconcat $(listE (runQueryBuilder_ (mapM_ reduceSegment xs))) |]

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

-- | Internal state for every builder
data BuilderState s p = BuilderState s Word [p]

-- |
class QueryCode s where
	type Code s

	appendCode :: s -> Code s -> s

	appendStringCode :: s -> String -> s

instance QueryCode B.ByteString where
	type Code B.ByteString = B.ByteString

	appendCode = B.append

	appendStringCode bs str = bs <> fromString str

instance QueryCode [Q Exp] where
	type Code [Q Exp] = Q Exp

	appendCode segments exp = segments ++ [exp]

	appendStringCode segments str = appendCode segments [e| fromString $(stringE str) |]

instance QueryCode String where
	type Code String = String

	appendCode segments code = segments ++ code

	appendStringCode = appendCode

-- | Can build @o@ using @s@ and @[p]@.
class QueryBuildable s p o | s p -> o where
	buildQuery :: s -> [p] -> o

instance QueryBuildable B.ByteString Value Query where
	buildQuery = Query

instance QueryBuildable String (Q Exp) (Q Exp) where
	buildQuery code params =
		[e| Query (fromString $(stringE code)) $(listE params) |]

instance QueryBuildable [Q Exp] (Q Exp) (Q Exp) where
	buildQuery codeSegments params =
		[e| Query (B.concat $(listE codeSegments)) $(listE params) |]

-- | Query builder
type QueryBuilder s p = State (BuilderState s p) ()

-- | Run query builder.
runQueryBuilder :: (QueryBuildable s p o, Monoid s) => QueryBuilder s p -> o
runQueryBuilder builder =
	buildQuery code params
	where
		BuilderState code _ params = execState builder (BuilderState mempty 1 [])

-- | Run query builder, return only the statement.
runQueryBuilder_ :: (Monoid s) => QueryBuilder s p -> s
runQueryBuilder_ builder =
	code
	where
		BuilderState code _ _ = execState builder (BuilderState mempty 1 [])

-- | Write code.
writeCode :: (QueryCode s) => Code s -> QueryBuilder s p
writeCode code =
	modify $ \ (BuilderState stmt idx params) ->
		BuilderState (appendCode stmt code) idx params

-- | Write string code.
writeStringCode :: (QueryCode s) => String -> QueryBuilder s p
writeStringCode code =
	modify $ \ (BuilderState stmt idx params) ->
		BuilderState (appendStringCode stmt code) idx params

-- | Add an identifier.
writeIdentifier :: (QueryCode s) => String -> QueryBuilder s p
writeIdentifier name =
	writeStringCode (quoteIdentifier name)

-- | Add an absolute identifier.
writeAbsIdentifier :: (QueryCode s) => String -> String -> QueryBuilder s p
writeAbsIdentifier ns name = do
	writeIdentifier ns
	writeStringCode "."
	writeIdentifier name

-- | Embed a parameter.
writeParam :: (QueryCode s) => p -> QueryBuilder s p
writeParam param = do
	modify $ \ (BuilderState code idx params) ->
		BuilderState (appendStringCode code ("$" ++ show idx)) (idx + 1) (params ++ [param])

-- | Embed a value parameter.
writeColumn :: (Column p, QueryCode s) => p -> QueryBuilder s Value
writeColumn param =
	writeParam (pack param)

-- | Do something between other builders.
intercalateBuilder :: QueryBuilder s p -> [QueryBuilder s p] -> QueryBuilder s p
intercalateBuilder x xs =
	sequence_ (intersperse x xs)
