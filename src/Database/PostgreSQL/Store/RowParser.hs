{-# LANGUAGE DataKinds,
             RankNTypes,
             KindSignatures,
             TypeOperators,
             TypeApplications,
             ScopedTypeVariables #-}

-- |
-- Module:     Database.PostgreSQL.Store.RowParser
-- Copyright:  (c) Ole Krüger 2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.RowParser (
	-- * Row parser
	RowParser,
	RowErrorLocation (..),
	RowErrorDetail (..),
	RowError (..),

	processResultWith,

	-- * Means of composition
	finish,
	cancel,
	withRowParser,
	skipColumns,
	nonNullCheck,

	-- * Default parsers
	parseValue,
	parseByteString
) where

import           GHC.TypeLits

import           Control.Monad.Except

import           Data.Proxy
import qualified Data.ByteString as B

import           Database.PostgreSQL.Store.Value

import qualified Database.PostgreSQL.LibPQ as P

-- | Location of an error
data RowErrorLocation = RowErrorLocation P.Column P.Row
	deriving (Show, Eq, Ord)

-- | Errors that occur during row parsing
data RowErrorDetail
	= TooFewColumns
		-- ^ Underlying 'RowParser' wants more columns than are currently present.
	| ColumnRejected Value
		-- ^ A column value could not be parsed.
	deriving (Show, Eq, Ord)

-- | An error that occured when parsing a row
data RowError = RowError RowErrorLocation RowErrorDetail
	deriving (Show, Eq, Ord)

-- | Shortcut for the internal monad transformer inside 'RowParser'
type M = ExceptT RowError IO

-- | Consumes @w@ columns of a result set row in order to produce an instance of @a@.
newtype RowParser (w :: Nat) a = RowParser { runProcessor :: P.Result -> P.Row -> P.Column -> M a }

instance Functor (RowParser w) where
	fmap f (RowParser action) =
		RowParser (\ result row col -> f <$> action result row col)

-- | Process the each row of the 'P.Result' with the given 'RowParser'.
processResultWith :: forall a n. (KnownNat n) => P.Result -> RowParser n a -> ExceptT RowError IO [a]
processResultWith result (RowParser run) = do
	cols <- lift (P.nfields result)
	when (cols < toEnum totalWidth) $
		throwError (RowError (RowErrorLocation 0 0) TooFewColumns)

	rows <- lift (P.ntuples result)
	forM [0 .. rows - 1] (\ row -> run result row 0)
	where
		totalWidth = fromIntegral (natVal @n Proxy)

-- | Terminate the parsing tree by returning the final result.
finish :: a -> RowParser 0 a
finish x = RowParser (\ _ _ _ -> pure x)

-- | Terminate the parsing tree with an error.
cancel :: RowErrorDetail -> RowParser 0 a
cancel detail = RowParser (\ _ row col -> throwError (RowError (RowErrorLocation col row) detail))

-- | Transform the result of another 'RowParser'. Similar to monadic bind. Also keeps track
-- of how many columns are needed in total.
withRowParser :: forall a v b w. (KnownNat v)
              => RowParser v a -> (a -> RowParser w b) -> RowParser (v + w) b
withRowParser proc func =
	RowParser $ \ result row col -> do
		x <- runProcessor proc result row col :: M a
		runProcessor (func x) result row (col + fromIntegral (natVal @v Proxy))

-- | Skip a number of columns.
skipColumns :: RowParser n ()
skipColumns = RowParser (\ _ _ _ -> pure ())

-- | Check if the following n columns are not @NULL@.
nonNullCheck :: Int -> RowParser 0 Bool
nonNullCheck n =
	RowParser $ \ result row col ->
		not . or <$> lift (forM [col .. col + (toEnum n - 1)] (P.getisnull result row))

-- | Row parser which extracts a single column 'Value'.
parseValue :: RowParser 1 Value
parseValue =
	RowParser $ \ result row col -> do
		mb <- lift (P.getvalue' result row col)
		case mb of
			Nothing  -> pure Null
			Just cnt -> (\ typ -> Value typ cnt) <$> lift (P.ftype result col)

-- | Extract the value of a cell. This parser will fail if value for that column is @NULL@.
parseByteString :: RowParser 1 B.ByteString
parseByteString =
	RowParser $ \ result row col -> do
		mb <- lift (P.getvalue' result row col)
		case mb of
			Just cnt -> pure cnt
			Nothing  -> throwError (RowError (RowErrorLocation col row) (ColumnRejected Null))
