-- |
-- Module:     Database.PostgreSQL.Store.Types
-- Copyright:  (c) Ole Krüger 2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.Types (
	-- * General
	Statement (..),
	Query (..),
	PrepQuery (..),

	toParam,
	toTypedParam,

	Oid (..),
	Format
) where

import           Text.Show.Functions             ()

import qualified Data.ByteString                 as B

import           Database.PostgreSQL.Store.Tuple

import           Database.PostgreSQL.LibPQ       (Format (Text), Oid (..))

-- | SQL statement
newtype Statement a = Statement B.ByteString
	deriving (Show, Eq, Ord)

-- | Query object
data Query a = Query {
	-- | SQL statement
	queryStatement :: B.ByteString,

	-- | Parameters
	queryParams    :: [Maybe (Oid, B.ByteString, Format)]
} deriving (Show, Eq, Ord)

-- | Preparable query object
data PrepQuery ts a = PrepQuery {
	-- | Name of the prepared statement
	prepName      :: B.ByteString,

	-- | SQL statement
	prepStatement :: B.ByteString,

	-- | Parameter type hints
	prepOids      :: [Oid],

	-- | Parameter generator
	prepParams    :: Tuple ts -> [Maybe (B.ByteString, Format)]
} deriving (Show)

-- | Attach 'Text' tag.
toParam :: B.ByteString -> (B.ByteString, Format)
toParam dat = (dat, Text)

-- | Attach 'Text' tag.
toTypedParam :: Oid -> B.ByteString -> (Oid, B.ByteString, Format)
toTypedParam typ dat = (typ, dat, Text)
