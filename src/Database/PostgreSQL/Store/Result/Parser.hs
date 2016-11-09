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

	rowNumber,
	columnNumber,

	fetchColumn,
	peekColumn,
	parseColumn,

	peekContents,
	fetchContents,
	parseContents,

	skipColumn,

	-- * Result Parser
	parseResult
) where

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State.Strict

import qualified Data.ByteString           as B

import qualified Database.PostgreSQL.LibPQ as P
import           Database.PostgreSQL.Store.Types

-- | Errors that occur during row parsing
data RowParseError
	= TooFewColumns P.Column P.Row
		-- ^ Underlying 'RowParser' wants more columns than are currently present.
	| ParseError P.Column P.Row TypedValue
		-- ^ A column value could not be parsed.
	deriving (Show, Eq, Ord)

-- |
data ResultInfo = ResultInfo P.Result P.Column

-- |
data RowInfo = RowInfo ResultInfo P.Row

-- | Row parser
newtype RowParser a =
	RowParser (ReaderT RowInfo (StateT P.Column (ExceptT RowParseError IO)) a)
	deriving (Functor, Applicative, Monad, MonadError RowParseError)

-- | Parse a single row.
parseRow :: RowParser a -> RowInfo -> ExceptT RowParseError IO a
parseRow (RowParser parser) rowInfo =
	evalStateT (runReaderT parser rowInfo) (P.Col 0)

-- | Retrieve the current row number.
rowNumber :: RowParser P.Row
rowNumber = RowParser (asks (\ (RowInfo _ row) -> row))

-- | Retrieve the current column number.
columnNumber :: RowParser P.Column
columnNumber = RowParser get

-- | Advance to next column without checking.
nextColumn :: RowParser ()
nextColumn =
	RowParser (modify (+ 1))

-- | Do something with the underlying result, row number and column number.
withColumn :: (P.Result -> P.Row -> P.Column -> RowParser a) -> RowParser a
withColumn action = do
	(RowInfo (ResultInfo result numColumns) row, col) <- RowParser ((,) <$> ask <*> get)

	if col < numColumns then
		action result row col
	else
		throwError (TooFewColumns col row)

-- | Fetch the 'TypedValue' associated with the current cell without advancing the cursor.
peekColumn :: RowParser TypedValue
peekColumn =
	withColumn $ \ result row col -> RowParser $ liftIO $
		TypedValue <$> P.ftype result col
		           <*> fmap (fmap Value) (P.getvalue' result row col)

-- | Fetch the type 'Oid' and value of the current cell. Also advances the cell cursor to the next
-- column.
fetchColumn :: RowParser TypedValue
fetchColumn =
	peekColumn <* nextColumn

-- | Fetch a column and parse it. Returning 'Nothing' from the provided parser function will cause
-- a 'ParseError' to be raised. Advances the cell cursor to the next column.
parseColumn :: (TypedValue -> Maybe a) -> RowParser a
parseColumn proc = do
	typedValue <- peekColumn
	case proc typedValue of
		Just x  -> x <$ nextColumn
		Nothing -> (ParseError <$> columnNumber <*> rowNumber <*> pure typedValue) >>= throwError

-- | Fetch the cell's contents without moving the cell cursor.
peekContents :: RowParser (Maybe B.ByteString)
peekContents =
	withColumn (\ result row col -> RowParser (liftIO (P.getvalue' result row col)))

-- | Like 'peekContents' but moves the cell cursor to the next column.
fetchContents :: RowParser (Maybe B.ByteString)
fetchContents =
	peekContents <* nextColumn

-- | Parse the contents of a column (only if present). Returning 'Nothing' from the provided parser
-- function will raise a 'ParserError'. When the cell is @NULL@, a 'ParserError' is raised aswell.
parseContents :: (B.ByteString -> Maybe a) -> RowParser a
parseContents proc = do
	value <- peekContents
	case value >>= proc of
		Just x  -> x <$ nextColumn
		Nothing -> (ParseError <$> columnNumber <*> rowNumber <*> peekColumn) >>= throwError

-- | Point cursor to the next column.
skipColumn :: RowParser ()
skipColumn = do
	(RowInfo (ResultInfo _ numColumns) row, col) <- RowParser ((,) <$> ask <*> get)

	if col < numColumns then
		nextColumn
	else
		throwError (TooFewColumns col row)

-- | Parse the result.
parseResult :: P.Result -> RowParser a -> ExceptT RowParseError IO [a]
parseResult result parser = do
	(resultInfo, numRows) <- liftIO $ do
		numColumns <- P.nfields result
		numRows <- P.ntuples result
		pure (ResultInfo result numColumns, numRows)

	forM [0 .. numRows - 1] (parseRow parser . RowInfo resultInfo)
