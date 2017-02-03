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

-- | Query object
data Query = Query {
	query2Statement :: B.ByteString,
	query2Params    :: [Value]
} deriving (Show, Eq, Ord)

-- | Preparable query object
data PrepQuery a = PrepQuery {
	prep2Name      :: B.ByteString,
	prep2Statement :: B.ByteString,
	prep2Params    :: a -> [Value]
} deriving (Show)
