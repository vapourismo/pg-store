-- |
-- Module:     Database.PostgreSQL.Store.Types
-- Copyright:  (c) Ole Krüger 2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.Types (
	-- * General
	Query (..),
	PrepQuery (..),

	toParam,
	toTypedParam,

	Oid (..),
	Format
) where

import           Text.Show.Functions ()

import qualified Data.ByteString as B

import           Database.PostgreSQL.Store.Tuple

import           Database.PostgreSQL.LibPQ (Oid (..), Format (Text))

-- | Query object
data Query a = Query {
	queryStatement :: B.ByteString,
	queryParams    :: [Maybe (Oid, B.ByteString, Format)]
} deriving (Show, Eq, Ord)

-- | Preparable query object
data PrepQuery ts a = PrepQuery {
	prepName      :: B.ByteString,
	prepStatement :: B.ByteString,
	prepOids      :: [Oid],
	prepParams    :: Tuple ts -> [Maybe (B.ByteString, Format)]
} deriving (Show)

-- |
toParam :: B.ByteString -> (B.ByteString, Format)
toParam dat = (dat, Text)

-- |
toTypedParam :: Oid -> B.ByteString -> (Oid, B.ByteString, Format)
toTypedParam typ dat = (typ, dat, Text)
