{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module:     Database.PostgreSQL.Store.Result.Parser
-- Copyright:  (c) Ole Krüger 2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.Result.Parser (
	-- * Row Parser
	RowParser,
	RowParseError (..),
	fetchColumn,
	peekColumn,
	parseColumn,
	skipColumn,
	columnNumber,
	parseContents,

	-- * Result Parser
	ResultParseError (..),
	parseResult
) where

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State.Strict

import           Data.Functor.Identity

import qualified Data.ByteString           as B

import qualified Database.PostgreSQL.LibPQ as P
import           Database.PostgreSQL.Store.Types

-- | Errors that occur during row parsing
data RowParseError
	= TooFewColumns P.Column
		-- ^ Underlying 'RowParser' wants more columns than are currently present.
	| ParseError P.Column TypedValue
		-- ^ A column value could not be parsed.
	deriving (Show, Eq, Ord)

-- | State for 'RowParser'
data Row = Row P.Column [TypedValue]

-- | Row parser
newtype RowParser a =
	RowParser (StateT Row (Except RowParseError) a)
	deriving (Functor, Applicative, Monad, MonadError RowParseError)

-- | Parse a single row.
parseRow :: RowParser a -> Row -> Except RowParseError a
parseRow (RowParser parser) =
	evalStateT parser

-- | Fetch the type 'Oid' and value of the current column.
fetchColumn :: RowParser TypedValue
fetchColumn = RowParser $ do
	Row columnNumber columns <- get
	case columns of
		[]            -> throwError (TooFewColumns columnNumber)
		column : rest -> column <$ put (Row (columnNumber + 1) rest)

-- | Fetch the 'TypedValue' associated with the current column without advancing the cursor.
peekColumn :: RowParser TypedValue
peekColumn = RowParser $ do
	Row columnNumber columns <- get
	case columns of
		[]         -> throwError (TooFewColumns columnNumber)
		column : _ -> pure column

-- | Fetch a column and parse it. Using this function allows you to associate the column number and
-- 'Oid' with the value that could not be parsed. See 'ParseError'.
parseColumn :: (TypedValue -> Maybe a) -> RowParser a
parseColumn proc = RowParser $ do
	Row columnNumber columns <- get
	case columns of
		[] -> throwError (TooFewColumns columnNumber)

		column : rest ->
			case proc column of
				Nothing -> throwError (ParseError columnNumber column)
				Just x  -> x <$ put (Row (columnNumber + 1) rest)

-- | Point cursor to the next column.
skipColumn :: RowParser ()
skipColumn = RowParser $ do
	Row columnNumber columns <- get
	case columns of
		[]       -> throwError (TooFewColumns columnNumber)
		_ : rest -> put (Row (columnNumber + 1) rest)

-- | Parse the contents of a column (only if present).
parseContents :: (B.ByteString -> Maybe a) -> RowParser a
parseContents proc =
	parseColumn $ \ (TypedValue _ mbValue) ->
		mbValue >>= proc . valueData

-- | Retrieve the current column number.
columnNumber :: RowParser P.Column
columnNumber =
	RowParser (gets (\ (Row columnNumber _) -> columnNumber))

-- | Result set
data ResultSet = ResultSet [P.Oid] [[Maybe Value]]

-- | Extract a 'ResultSet' from the 'Result'.
buildResultSet :: P.Result -> IO ResultSet
buildResultSet result = do
	rows <- P.ntuples result
	columns <- P.nfields result

	-- No need to check whether rows or columns are greater than 0, because [0 .. -1] is [].

	ResultSet <$> forM [0 .. columns - 1] (P.ftype result)
	          <*> forM [0 .. rows - 1] (forM [0 .. columns - 1] . getValue result)

	where
		getValue result row column =
			fmap Value <$> P.getvalue' result row column

-- | Error that occurs during result parsing
data ResultParseError = RowParseError P.Row RowParseError
	deriving (Show, Eq, Ord)

-- | Parse the entire result set.
parseResultSet :: ResultSet -> RowParser a -> Except ResultParseError [a]
parseResultSet (ResultSet types values) parser =
	forM (zip values [0 .. P.toRow (length values - 1)]) $ \ (row, rowNumber) ->
		withExcept (RowParseError rowNumber) $
			parseRow parser (Row 0 (zipWith TypedValue types row))

-- | Parse the result.
parseResult :: P.Result -> RowParser a -> ExceptT ResultParseError IO [a]
parseResult result parser = do
	resultSet <- liftIO (buildResultSet result)
	mapExceptT (pure . runIdentity) (parseResultSet resultSet parser)
