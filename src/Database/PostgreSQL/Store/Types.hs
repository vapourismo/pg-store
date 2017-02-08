-- |
-- Module:     Database.PostgreSQL.Store.Types
-- Copyright:  (c) Ole Krüger 2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.Types (
	-- * General
	Value (..),
	Query (..),
	PrepQuery (..)
) where

import           Text.Show.Functions ()

import qualified Data.ByteString as B

import           Database.PostgreSQL.Store.Parameters

import           Database.PostgreSQL.LibPQ (Oid (..))

-- | Data that will be sent to the database as parameters to a request
--
-- Payload is always in text format.
data Value
	= Value Oid B.ByteString -- ^ Payload with type hint
	| Null                   -- ^ Equivalent to @NULL@
	deriving (Show, Eq, Ord)

-- | Query object
data Query a = Query {
	queryStatement :: B.ByteString,
	queryParams    :: [Value]
} deriving (Show, Eq, Ord)

-- | Preparable query object
data PrepQuery p a = PrepQuery {
	prepName      :: B.ByteString,
	prepStatement :: B.ByteString,
	prepOids      :: [Oid],
	prepParams    :: Parameters p -> [Value]
} deriving (Show)
