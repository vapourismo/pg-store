{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module:     Database.PostgreSQL.Store.Result
-- Copyright:  (c) Ole Krüger 2015-2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.Result (
	ResultError (..),

	ResultProcessor,
	processResult,
	processOneResult,

	skipColumn,
	unpackColumn
) where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State.Strict
import           Control.Monad.Trans.Except

import           Data.Proxy

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
	deriving (Functor, Applicative, Monad, MonadIO)

-- | Throw a nerror.
raiseResultError :: ResultError -> ResultProcessor a
raiseResultError err = ResultProcessor (lift (lift (throwE err)))

-- | Read information packed into the "ReaderT".
readInfo :: ResultProcessor (P.Result, P.Row, P.Column)
readInfo = ResultProcessor (lift ask)

-- | Get the current column.
getColumn :: ResultProcessor P.Column
getColumn = ResultProcessor get

-- | Set the new column.
setColumn :: P.Column -> ResultProcessor ()
setColumn col = ResultProcessor (put col)

-- | Skip the current column.
skipColumn :: ResultProcessor ()
skipColumn =
	ResultProcessor (modify (+ 1))

-- | Unpack a column.
unpackColumn :: (Column a) => ResultProcessor a
unpackColumn = do
	-- Gather information
	col <- getColumn
	(res, row, numCol) <- readInfo

	-- Make sure we're not trying to unpack a non-existing column
	when (col >= numCol) (raiseResultError (TooFewColumnsError numCol))

	-- Retrieve column-specific information
	(typ, fmt, mbData) <- ResultProcessor . liftIO $
		(,,) <$> P.ftype res col
		     <*> P.fformat res col
		     <*> P.getvalue' res row col

	-- Try to unpack the value
	case unpack (maybe NullValue (\ dat -> Value typ dat fmt) mbData) of
		Just ret ->
			ret <$ setColumn (col + 1)

		nothing ->
			raiseResultError (UnpackError row col typ fmt)
	where
		makeProxy :: Maybe a -> Proxy a
		makeProxy _ = Proxy

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
