{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module:     Database.PostgreSQL.Store.Result.Parser
-- Copyright:  (c) Ole Krüger 2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.Result.Parser (
	-- * Row Parser
	RowParser,
	fetchColumn,
	parseColumn,
	skipColumn,
	columnNumber,

	-- * Result Parser
	parseResult
) where

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State.Strict

import           Data.Functor.Identity

import qualified Database.PostgreSQL.LibPQ as P
import           Database.PostgreSQL.Store.Types

-- | Errors that occur during row parsing
data RowParseError
	= TooFewColumns P.Column
		-- ^ Underlying 'RowParser' wants more columns than are currently present.
	| ParseError P.Column P.Oid Value
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
parseRow (RowParser parser) row =
	evalStateT parser row

-- | Fetch the type 'Oid' and value of the current column.
fetchColumn :: RowParser TypedValue
fetchColumn = RowParser $ do
	Row columnNumber columns <- get
	case columns of
		[]            -> throwError (TooFewColumns columnNumber)
		column : rest -> column <$ put (Row (columnNumber + 1) rest)

-- | Fetch a column and parse it. Using this function allows you to associate the column number and
-- 'Oid' with the value that could not be parsed.
parseColumn :: (P.Oid -> Value -> Maybe a) -> RowParser a
parseColumn proc = RowParser $ do
	Row columnNumber columns <- get
	case columns of
		[] -> throwError (TooFewColumns columnNumber)

		(typ, val) : rest ->
			case proc typ val of
				Nothing -> throwError (ParseError columnNumber typ val)
				Just x  -> x <$ put (Row (columnNumber + 1) rest)

-- | Point cursor to the next column.
skipColumn :: RowParser ()
skipColumn = RowParser $ do
	Row columnNumber columns <- get
	case columns of
		[]       -> throwError (TooFewColumns columnNumber)
		_ : rest -> put (Row (columnNumber + 1) rest)

-- | Retrieve the current column number.
columnNumber :: RowParser P.Column
columnNumber =
	RowParser (gets (\ (Row columnNumber _) -> columnNumber))

-- | Result set
data ResultSet = ResultSet [P.Oid] [[Value]]
	deriving (Show, Eq)

-- | Extract a 'ResultSet' from the 'Result'.
buildResultSet :: P.Result -> IO ResultSet
buildResultSet result = do
	rows <- P.ntuples result
	columns <- P.nfields result

	-- No need to check whether rows or columns are greater than 0, because [0 .. -1] is [].

	ResultSet <$> forM [0 .. columns - 1] (P.ftype result)
	          <*> forM [0 .. rows - 1] (forM [0 .. columns - 1] . P.getvalue' result)

-- | Error that occurs during result parsing
data ResultParseError = ResultParseError P.Row RowParseError
	deriving (Show, Eq, Ord)

-- | Parse the entire result set.
parseResultSet :: RowParser a -> ResultSet -> Except ResultParseError [a]
parseResultSet parser (ResultSet types values) =
	forM (zip values [0 .. P.toRow (length values - 1)]) $ \ (row, rowNumber) ->
		withExcept (ResultParseError rowNumber) (parseRow parser (Row 0 (zip types row)))

-- | Parse the result.
parseResult :: RowParser a -> P.Result -> ExceptT ResultParseError IO [a]
parseResult parser result = do
	resultSet <- liftIO (buildResultSet result)
	mapExceptT (pure . runIdentity) (parseResultSet parser resultSet)
