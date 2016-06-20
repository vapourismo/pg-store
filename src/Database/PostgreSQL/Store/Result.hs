{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module:     Database.PostgreSQL.Store.Result
-- Copyright:  (c) Ole Krüger 2015-2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.Result (
	-- * Result processor
	ResultError (..),
	ResultProcessor,
	processResult,
	processOneResult,

	skipColumn,
	unpackColumn
) where

import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Control.Monad.Except

import qualified Database.PostgreSQL.LibPQ as P
import           Database.PostgreSQL.Store.Columns

-- | Error that occured during result processing
data ResultError
	= TooFewColumnsError P.Column
	  -- ^ Occurs when you're trying to access a column that does not exist.
	| UnpackError P.Row P.Column P.Oid P.Format
	  -- ^ The value at a given row and column could not be unpacked.
	deriving (Show, Eq)

-- | Result processor
newtype ResultProcessor a =
	ResultProcessor (StateT P.Column (ReaderT (P.Result, P.Row, P.Column) (ExceptT ResultError IO)) a)
	deriving (Functor, Applicative, Monad, MonadIO, MonadError ResultError)

-- | Move cursor to the next column.
skipColumn :: ResultProcessor ()
skipColumn =
	ResultProcessor (modify (+ 1))

-- | Unpack the current column and move the cursor to the next column.
unpackColumn :: (Column a) => ResultProcessor a
unpackColumn = do
	-- Gather information
	col <- ResultProcessor get
	(res, row, numCol) <- ResultProcessor ask

	-- Make sure we're not trying to unpack a non-existing column
	when (col >= numCol) (throwError (TooFewColumnsError numCol))

	-- Retrieve column-specific information
	(typ, fmt, mbData) <- liftIO $
		(,,) <$> P.ftype res col
		     <*> P.fformat res col
		     <*> P.getvalue' res row col

	-- Try to unpack the value
	case unpack (maybe NullValue (\ dat -> Value typ dat fmt) mbData) of
		Just ret -> ret <$ ResultProcessor (put (col + 1))
		Nothing  -> throwError (UnpackError row col typ fmt)

-- | Process the entire result set.
processResult :: P.Result -> ResultProcessor a -> ExceptT ResultError IO [a]
processResult res (ResultProcessor proc) = do
	rows <- liftIO (P.ntuples res)
	cols <- liftIO (P.nfields res)

	-- Iterate over each row number and run the row processor
	forM [0 .. rows - 1] $ \ row ->
		runReaderT (evalStateT proc 0) (res, row, cols)

-- | Process one row of the result set.
processOneResult :: P.Result -> ResultProcessor a -> ExceptT ResultError IO (Maybe a)
processOneResult res (ResultProcessor proc) = do
	rows <- liftIO (P.ntuples res)
	cols <- liftIO (P.nfields res)

	if rows > 0 then
		Just <$> runReaderT (evalStateT proc 0) (res, 0, cols)
	else
		pure Nothing
