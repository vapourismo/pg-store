{-# LANGUAGE OverloadedStrings, TemplateHaskell, BangPatterns #-}

-- |
-- Module:     Database.PostgreSQL.Store.Query
-- Copyright:  (c) Ole Krüger 2015-2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.Query (
	Query (..),

	SelectorElement (..),
	QueryTable (..),

	pgsq,
	pgss,

	tableNameE,
	tableIDNameE,
	tableAbsoluteIDNameE,
	makeTableSelectorsE,
) where

import           Language.Haskell.TH
import           Language.Haskell.TH.Quote

import           Control.Applicative
import           Control.Monad.Trans
import           Control.Monad.State

import           Data.List
import           Data.Proxy
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString    as B

import           Data.Char
import           Data.Attoparsec.Text

import           Database.PostgreSQL.Store.Columns

-- | Query including statement and parameters.
--   Use the 'pgsq' quasi-quoter to conveniently create queries.
data Query = Query {
	-- | Statement
	queryStatement :: !B.ByteString,

	-- | Parameters
	queryParams :: ![Value]
} deriving (Show, Eq, Ord)

-- | A selector element
data SelectorElement
	= SelectorField String
	  -- ^ Select a field.
	| SelectorSpecial String
	  -- ^ Select a special expression.

-- | This type can be used as a table in a query.
class QueryTable a where
	-- | Table name
	tableName :: Proxy a -> String

	-- | Name of the ID field
	tableIDName :: Proxy a -> String

	-- | Selectors needed to retrieve all fields necessary to construct the type
	tableSelectors :: Proxy a -> [SelectorElement]

-- | Generate table name expression.
tableNameE :: Name -> Q Exp
tableNameE typName =
	[e| show (tableName (Proxy :: Proxy $(conT typName))) |]

-- | Generate table ID name expression
tableIDNameE :: Name -> Q Exp
tableIDNameE typName =
	[e| show (tableIDName (Proxy :: Proxy $(conT typName))) |]

-- | Generate absolute table ID name expression.
tableAbsoluteIDNameE :: Name -> Q Exp
tableAbsoluteIDNameE typName =
	[e| $(tableNameE typName) ++ "." ++ $(tableIDNameE typName) |]

-- | Generate the list of expression used as selector.
makeTableSelectors :: (QueryTable a) => Proxy a -> String
makeTableSelectors proxy =
	intercalate ", " (map makeElement (tableSelectors proxy))
	where
		makeElement (SelectorField name)   = show (tableName proxy) ++ "." ++ show name
		makeElement (SelectorSpecial expr) = expr

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
	| SOther Char

-- | Name
name :: Parser String
name =
	(:) <$> (letter <|> char '_') <*> many (satisfy isAlphaNum <|> char '_')

-- | Type name
typeName :: Parser String
typeName =
	(:) <$> satisfy isUpper <*> many (satisfy isAlphaNum <|> char '_')

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
	              char '#' >> SSelector <$> typeName,
	              char '@' >> STable <$> typeName,
	              char '&' >> SIdentifier <$> typeName,
	              char '$' >> SVariable <$> name,
	              SOther <$> anyChar])

-- | Reduce segments in order to resolve names and collect query parameters.
reduceSegment :: Segment -> StateT (Int, [Exp]) Q Exp
reduceSegment seg =
	case seg of
		SOther o ->
			lift (stringE [o])

		SQuote delim cnt ->
			lift (stringE (delim : cnt ++ [delim]))

		SVariable varName -> do
			mbName <- lift (lookupValueName varName)
			case mbName of
				Just name -> do
					-- Generate the pack expression
					lit <- lift [e| pack $(varE name) |]

					-- Register parameter
					(numParams, params) <- get
					put (numParams + 1, params ++ [lit])

					-- Leave only the placeholder
					lift (stringE ("$" ++ show (numParams + 1)))

				Nothing ->
					lift (fail ("\ESC[34m" ++ varName ++ "\ESC[0m does not refer to anything"))

		STable tableName -> lift $ do
			mbName <- lookupTypeName tableName
			maybe (fail ("\ESC[34m" ++ tableName ++ "\ESC[0m does not refer to anything"))
			      tableNameE -- Replace with table name
			      mbName

		SSelector tableName -> lift $ do
			mbName <- lookupTypeName tableName
			maybe (fail ("\ESC[34m" ++ tableName ++ "\ESC[0m does not refer to anything"))
			      makeTableSelectorsE -- Replace with selector list
			      mbName

		SIdentifier tableName -> lift $ do
			mbName <- lookupTypeName tableName
			maybe (fail ("\ESC[34m" ++ tableName ++ "\ESC[0m does not refer to anything"))
			      tableAbsoluteIDNameE -- Replace with absolute ID name
			      mbName

-- | Parse quasi-quoted query.
parseStoreQueryE :: String -> Q Exp
parseStoreQueryE code = do
	case parseOnly (segments <* endOfInput) (T.pack code) of
		Left msg ->
			fail msg

		Right xs -> do
			(parts, (_, params)) <- runStateT (mapM reduceSegment xs) (0, [])
			[e| Query {
			        queryStatement = T.encodeUtf8 (T.pack (concat $(pure (ListE parts)))),
			        queryParams    = $(pure (ListE params))
			    } |]

-- |
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
			parts <- evalStateT (mapM reduceSegment xs) (0, [])
			[e| concat $(pure (ListE parts)) |]

-- | Just like "pgsq" but only produces the statement associated with the query. Referenced
--   variables are not inlined, they are simply dismissed.
pgss :: QuasiQuoter
pgss =
	QuasiQuoter {
		quoteExp  = parseStoreStatementE,
		quotePat  = const (fail "Cannot use 'pgss' in pattern"),
		quoteType = const (fail "Cannot use 'pgss' in type"),
		quoteDec  = const (fail "Cannot use 'pgss' in declaration")
	}
