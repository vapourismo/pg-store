-- |
-- Module:     Database.PostgreSQL.Store.Types
-- Copyright:  (c) Ole Krüger 2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.Types (
	-- * General
	Value (..),
	TypedValue (..)
) where

import qualified Data.ByteString           as B
import qualified Database.PostgreSQL.LibPQ as P

-- | Value of a cell in the result set
newtype Value = Value { valueData :: B.ByteString }
	deriving (Show, Eq, Ord)

-- | Value and type 'Oid' of a cell in the result set
data TypedValue = TypedValue P.Oid (Maybe Value)
	deriving (Show, Eq, Ord)
