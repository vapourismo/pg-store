{-# LANGUAGE ExistentialQuantification, StandaloneDeriving #-}

module Database.PostgreSQL.Store.Result (
	-- *
	ResultError (..),
	ResultProcessor,
	runResultProcessor,
	columnNumber,
	columnType,
	columnFormat,
	columnInfo,
	foreachRow,
	cellData,
	cellValue,
	unpackCellValue,

	-- * ResultRow
	ResultRow (..)
) where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Except

import           Data.List
import qualified Data.ByteString           as B

import qualified Database.PostgreSQL.LibPQ as P
import           Database.PostgreSQL.Store.Columns

-- | Error that occured during result processing
data ResultError
	= ColumnMissing B.ByteString
	| ColumnDataMissing P.Row P.Column
	| forall a. ValueError P.Row P.Column P.Oid P.Format (ColumnDescription a)

deriving instance Show ResultError

-- | Result processor
type ResultProcessor = ReaderT P.Result (ExceptT ResultError IO)

-- | Process the result.
runResultProcessor :: P.Result -> ResultProcessor a -> ExceptT ResultError IO a
runResultProcessor = flip runReaderT

-- | Get the column number for a column name.
columnNumber :: B.ByteString -> ResultProcessor P.Column
columnNumber name = do
	result <- ask
	lift (ExceptT (maybe (Left (ColumnMissing name)) pure <$> P.fnumber result name))

-- | Get the type of a column.
columnType :: P.Column -> ResultProcessor P.Oid
columnType col = do
	result <- ask
	lift (lift (P.ftype result col))

-- | Get the format of a column.
columnFormat :: P.Column -> ResultProcessor P.Format
columnFormat col = do
	result <- ask
	lift (lift (P.fformat result col))

-- | Get information about a column.
columnInfo :: B.ByteString -> ResultProcessor (P.Column, P.Oid, P.Format)
columnInfo name = do
	col <- columnNumber name
	(,,) col <$> columnType col <*> columnFormat col

-- | Do something for each row.
foreachRow :: (P.Row -> ResultProcessor a) -> ResultProcessor [a]
foreachRow rowProcessor = do
	result <- ask
	numRows <- lift (lift (P.ntuples result))
	mapM rowProcessor [0 .. numRows - 1]

-- | Get cell data.
cellData :: P.Row -> P.Column -> ResultProcessor B.ByteString
cellData row col = do
	result <- ask
	lift (ExceptT (maybe (Left (ColumnDataMissing row col)) pure <$> P.getvalue' result row col))

-- | Get cell value.
cellValue :: P.Row -> (P.Column, P.Oid, P.Format) -> ResultProcessor Value
cellValue row (col, oid, fmt) = do
	Value oid <$> cellData row col <*> pure fmt

-- | Unpack cell value.
unpackCellValue :: (Column a) => P.Row -> (P.Column, P.Oid, P.Format) -> ResultProcessor a
unpackCellValue row info = do
	value <- cellValue row info
	lift (ExceptT (make columnDescription (unpack value)))
	where
		(col, oid, fmt) = info

		valueError desc =
			pure (Left (ValueError row col oid fmt desc))

		make :: ColumnDescription a -> Maybe a -> IO (Either ResultError a)
		make desc =
			maybe (valueError desc) (pure . pure)

-- | Result row
class ResultRow a where
	-- | Extract rows from the given result.
	resultProcessor :: ResultProcessor [a]

instance ResultRow () where
	resultProcessor = foreachRow (const (pure ()))

instance (ResultRow a, ResultRow b) => ResultRow (a, b) where
	resultProcessor =
		zip <$> resultProcessor
		    <*> resultProcessor

instance (ResultRow a, ResultRow b, ResultRow c) => ResultRow (a, b, c) where
	resultProcessor =
		zip3 <$> resultProcessor
		     <*> resultProcessor
		     <*> resultProcessor

instance (ResultRow a, ResultRow b, ResultRow c, ResultRow d) => ResultRow (a, b, c, d) where
	resultProcessor =
		zip4 <$> resultProcessor
		     <*> resultProcessor
		     <*> resultProcessor
		     <*> resultProcessor

instance (ResultRow a, ResultRow b, ResultRow c, ResultRow d, ResultRow e) => ResultRow (a, b, c, d, e) where
	resultProcessor =
		zip5 <$> resultProcessor
		     <*> resultProcessor
		     <*> resultProcessor
		     <*> resultProcessor
		     <*> resultProcessor

instance (ResultRow a, ResultRow b, ResultRow c, ResultRow d, ResultRow e, ResultRow f) => ResultRow (a, b, c, d, e, f) where
	resultProcessor =
		zip6 <$> resultProcessor
		     <*> resultProcessor
		     <*> resultProcessor
		     <*> resultProcessor
		     <*> resultProcessor
		     <*> resultProcessor

instance (ResultRow a, ResultRow b, ResultRow c, ResultRow d, ResultRow e, ResultRow f, ResultRow g) => ResultRow (a, b, c, d, e, f, g) where
	resultProcessor =
		zip7 <$> resultProcessor
		     <*> resultProcessor
		     <*> resultProcessor
		     <*> resultProcessor
		     <*> resultProcessor
		     <*> resultProcessor
		     <*> resultProcessor
