-- |
-- Module:     Database.PostgreSQL.Store.Types
-- Copyright:  (c) Ole Krüger 2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.Types (
	-- * General
	Value,
	TypedValue
) where

import qualified Data.ByteString           as B
import qualified Database.PostgreSQL.LibPQ as P

-- | Value of a cell in the result set
type Value = Maybe B.ByteString

-- | Value and type 'Oid' of a cell in the result set
type TypedValue = (P.Oid, Value)
