-- |
-- Module:     Database.PostgreSQL.Store.Types
-- Copyright:  (c) Ole Krüger 2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.Types (
	-- * General
	Query (..),
	PrepQuery (..)
) where

import           Text.Show.Functions ()

import qualified Data.ByteString as B

import           Database.PostgreSQL.Store.Value
import           Database.PostgreSQL.Store.Parameters

-- | Query object
data Query a = Query {
	queryStatement :: B.ByteString,
	queryParams    :: [Value]
} deriving (Show, Eq, Ord)

-- | Preparable query object
data PrepQuery p a = PrepQuery {
	prepName      :: B.ByteString,
	prepStatement :: B.ByteString,
	prepParams    :: Parameters p -> [Value]
} deriving (Show)
