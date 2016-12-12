{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module:     Database.PostgreSQL.Store.RowParser
-- Copyright:  (c) Ole Krüger 2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.RowParser (
	-- * Row Parser
	RowParser,
	RowErrorLocation (..),
	RowErrorDetail (..),
	RowError (..),

	rowNumber,
	columnNumber,
	columnsLeft,

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

-- | Location of an error
data RowErrorLocation = RowErrorLocation P.Column P.Row
	deriving (Show, Eq, Ord)

-- | Errors that occur during row parsing
data RowErrorDetail
	= TooFewColumns
		-- ^ Underlying 'RowParser' wants more columns than are currently present.
	| ColumnRejected TypedValue
		-- ^ A column value could not be parsed.
	| ContentsRejected (Maybe B.ByteString)
		-- ^ The contents of a column could not be parsed.
	deriving (Show, Eq, Ord)

-- | An error that occured when parsing a row.
data RowError = RowError RowErrorLocation RowErrorDetail
	deriving (Show, Eq, Ord)

-- | Static result information
data ResultInfo = ResultInfo P.Result -- Result to operate on
                             P.Column -- Total number of columns

-- | Static row information
data RowInput = RowInput ResultInfo -- Result info
                         P.Row      -- Current row to operate on

-- | Row parser
newtype RowParser a =
	RowParser (ReaderT RowInput (StateT P.Column (ExceptT RowError IO)) a)
	deriving (Functor, Applicative, Monad, MonadError RowError)

-- | Parse a single row.
parseRow :: RowParser a -> RowInput -> ExceptT RowError IO a
parseRow (RowParser parser) rowInfo =
	evalStateT (runReaderT parser rowInfo) (P.Col 0)

-- | Retrieve the current row number.
rowNumber :: RowParser P.Row
rowNumber = RowParser (asks (\ (RowInput _ row) -> row))

-- | Retrieve the current column number.
columnNumber :: RowParser P.Column
columnNumber = RowParser get

-- | Retrieve the number of columns left.
columnsLeft :: RowParser P.Column
columnsLeft = RowParser $ do
	RowInput (ResultInfo _ numCols) _ <- ask
	curCol <- get
	pure (numCols - curCol)

-- | Advance to next column without checking.
nextColumn :: RowParser ()
nextColumn =
	RowParser (modify (+ 1))

-- | Do something with the underlying result, row number and column number.
withColumn :: (P.Result -> P.Row -> P.Column -> RowParser a) -> RowParser a
withColumn action = do
	(RowInput (ResultInfo result numColumns) row, col) <- RowParser ((,) <$> ask <*> get)

	if col < numColumns then
		action result row col
	else
		throwError (RowError (RowErrorLocation col row) TooFewColumns)

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
-- a 'ColumnRejected' to be raised. Advances the cell cursor to the next column.
parseColumn :: (TypedValue -> Maybe a) -> RowParser a
parseColumn proc = do
	typedValue <- peekColumn
	case proc typedValue of
		Just x  -> x <$ nextColumn
		Nothing -> do
			col <- columnNumber
			row <- rowNumber
			throwError (RowError (RowErrorLocation col row) (ColumnRejected typedValue))

-- | Fetch the cell's contents without moving the cell cursor.
peekContents :: RowParser (Maybe B.ByteString)
peekContents =
	withColumn (\ result row col -> RowParser (liftIO (P.getvalue' result row col)))

-- | Like 'peekContents' but moves the cell cursor to the next column.
fetchContents :: RowParser (Maybe B.ByteString)
fetchContents =
	peekContents <* nextColumn

-- | Parse the contents of a column (only if present). Returning 'Nothing' from the provided parser
-- function will raise a 'ContentsRejected'. When the cell is @NULL@, a 'ContentsRejected' is raised
-- aswell.
parseContents :: (B.ByteString -> Maybe a) -> RowParser a
parseContents proc = do
	value <- peekContents
	case value >>= proc of
		Just x  -> x <$ nextColumn
		Nothing -> do
			col <- columnNumber
			row <- rowNumber
			throwError (RowError (RowErrorLocation col row) (ContentsRejected value))

-- | Point cursor to the next column.
skipColumn :: RowParser ()
skipColumn = do
	(RowInput (ResultInfo _ numColumns) row, col) <- RowParser ((,) <$> ask <*> get)

	if col < numColumns then
		nextColumn
	else
		throwError (RowError (RowErrorLocation col row) TooFewColumns)

-- | Parse the result.
parseResult :: P.Result -> RowParser a -> ExceptT RowError IO [a]
parseResult result parser = do
	(resultInfo, numRows) <- liftIO $ do
		numColumns <- P.nfields result
		numRows <- P.ntuples result
		pure (ResultInfo result numColumns, numRows)

	forM [0 .. numRows - 1] (parseRow parser . RowInput resultInfo)
