-- |
-- Module:     Database.PostgreSQL.Store.Result
-- Copyright:  (c) Ole Krüger 2015-2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.Result (
	-- *
	ResultError (..),
	ResultProcessor,
	ColumnInfo,
	runResultProcessor,
	columnNumber,
	columnType,
	columnFormat,
	columnInfo,
	foreachRow,
	cellValue,
	unpackCellValue,

	-- * Result
	Result (..),
	RawRow (..)
) where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Except

import           Data.List
import           Data.Typeable
import qualified Data.ByteString           as B

import qualified Database.PostgreSQL.LibPQ as P
import           Database.PostgreSQL.Store.Columns

-- | Error that occured during result processing
data ResultError
	= ColumnMissing B.ByteString
	| ValueError P.Row P.Column P.Oid P.Format ColumnDescription
	deriving (Show, Eq)

-- | Result processor
type ResultProcessor = ReaderT P.Result (ExceptT ResultError IO)

-- | Column information
type ColumnInfo = (P.Column, P.Oid, P.Format)

-- | Process the result.
runResultProcessor :: P.Result -> ResultProcessor a -> ExceptT ResultError IO a
runResultProcessor = flip runReaderT

-- | Get the number of columns.
numColumns :: ResultProcessor P.Column
numColumns = do
	result <- ask
	lift (lift (P.nfields result))

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
columnInfo :: B.ByteString -> ResultProcessor ColumnInfo
columnInfo name = do
	col <- columnNumber name
	(,,) col <$> columnType col <*> columnFormat col

-- | Do something for each row.
foreachRow :: (P.Row -> ResultProcessor a) -> ResultProcessor [a]
foreachRow rowProcessor = do
	result <- ask
	numRows <- lift (lift (P.ntuples result))
	mapM rowProcessor [0 .. numRows - 1]

-- | Get cell value.
cellValue :: P.Row -> ColumnInfo -> ResultProcessor Value
cellValue row (col, oid, fmt) = do
	result <- ask
	value <- lift (lift (P.getvalue' result row col))

	case value of
		Just dat -> pure (Value oid dat fmt)
		Nothing  -> pure NullValue

-- | Get cell value.
cellValue' :: P.Row -> P.Column -> ResultProcessor Value
cellValue' row col = do
	result <- ask
	(oid, fmt, value) <- lift (lift ((,,) <$> P.ftype result col
	                                      <*> P.fformat result col
	                                      <*> P.getvalue' result row col))

	case value of
		Just dat -> pure (Value oid dat fmt)
		Nothing  -> pure NullValue


-- | Unpack cell value.
unpackCellValue :: (Column a) => P.Row -> ColumnInfo -> ResultProcessor a
unpackCellValue row info =
	withProxy Proxy
	where
		withProxy :: (Column a) => Proxy a -> ResultProcessor a
		withProxy proxy = do
			value <- cellValue row info
			lift (ExceptT (make (describeColumn proxy) (unpack value)))

		(col, oid, fmt) = info

		valueError desc =
			pure (Left (ValueError row col oid fmt desc))

		make desc =
			maybe (valueError desc) (pure . pure)

-- | Result row
class Result a where
	-- | Extract rows from the given result.
	resultProcessor :: ResultProcessor [a]

instance Result () where
	resultProcessor = foreachRow (const (pure ()))

instance (Result a, Result b) => Result (a, b) where
	resultProcessor =
		zip <$> resultProcessor
		    <*> resultProcessor

instance (Result a, Result b, Result c) => Result (a, b, c) where
	resultProcessor =
		zip3 <$> resultProcessor
		     <*> resultProcessor
		     <*> resultProcessor

instance (Result a, Result b, Result c, Result d) => Result (a, b, c, d) where
	resultProcessor =
		zip4 <$> resultProcessor
		     <*> resultProcessor
		     <*> resultProcessor
		     <*> resultProcessor

instance (Result a, Result b, Result c, Result d, Result e) => Result (a, b, c, d, e) where
	resultProcessor =
		zip5 <$> resultProcessor
		     <*> resultProcessor
		     <*> resultProcessor
		     <*> resultProcessor
		     <*> resultProcessor

instance (Result a, Result b, Result c, Result d, Result e, Result f) => Result (a, b, c, d, e, f) where
	resultProcessor =
		zip6 <$> resultProcessor
		     <*> resultProcessor
		     <*> resultProcessor
		     <*> resultProcessor
		     <*> resultProcessor
		     <*> resultProcessor

instance (Result a, Result b, Result c, Result d, Result e, Result f, Result g) => Result (a, b, c, d, e, f, g) where
	resultProcessor =
		zip7 <$> resultProcessor
		     <*> resultProcessor
		     <*> resultProcessor
		     <*> resultProcessor
		     <*> resultProcessor
		     <*> resultProcessor
		     <*> resultProcessor

-- | A row containing a list of all column values.
newtype RawRow = RawRow [Value]
	deriving (Show, Eq, Ord)

instance Result RawRow where
	resultProcessor = do
		ncols <- numColumns
		foreachRow $ \ row ->
			RawRow <$> forM [0 .. ncols - 1] (cellValue' row)
