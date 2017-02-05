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
	-- * Row Parser
	RowParser,
	RowErrorLocation (..),
	RowErrorDetail (..),
	RowError (..),

	processResultWith,

	finish,
	cancel,
	withRowParser,

	parseValue
) where

import           GHC.TypeLits

import           Control.Monad.Except

import           Data.Proxy

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

-- | An error that occured when parsing a row.
data RowError = RowError RowErrorLocation RowErrorDetail
	deriving (Show, Eq, Ord)

-- |
type M = ExceptT RowError IO

-- |
newtype RowParser (w :: Nat) a = RowParser { runProcessor :: P.Result -> P.Row -> P.Column -> M a }

-- |
processResultWith :: forall a n. (KnownNat n) => P.Result -> RowParser n a -> M [a]
processResultWith result (RowParser run) = do
	cols <- lift (P.nfields result)
	when (cols < toEnum totalWidth) $
		throwError (RowError (RowErrorLocation 0 0) TooFewColumns)

	rows <- lift (P.ntuples result)
	forM [0 .. rows - 1] (\ row -> run result row 0)
	where
		totalWidth = fromIntegral (natVal @n Proxy)

-- |
finish :: a -> RowParser 0 a
finish x = RowParser (\ _ _ _ -> pure x)

-- |
cancel :: RowErrorDetail -> RowParser 0 a
cancel detail = RowParser (\ _ row col -> throwError (RowError (RowErrorLocation col row) detail))

-- |
withRowParser :: forall a v b w. (KnownNat v)
              => RowParser v a -> (a -> RowParser w b) -> RowParser (v + w) b
withRowParser proc func =
	RowParser $ \ result row col -> do
		x <- runProcessor proc result row col :: M a
		runProcessor (func x) result row (col + fromIntegral (natVal @v Proxy))

-- |
parseValue :: RowParser 1 Value
parseValue =
	RowParser $ \ result row col -> do
		mb <- lift (P.getvalue' result row col)
		case mb of
			Nothing  -> pure Null
			Just cnt -> (\ typ -> Value typ cnt) <$> lift (P.ftype result col)
