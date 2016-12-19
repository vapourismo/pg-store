-- |
-- Module:     Database.PostgreSQL.Store.Query
-- Copyright:  (c) Ole Krüger 2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
--
-- This module acts solely as a re-export unit.
module Database.PostgreSQL.Store.Query (
	-- * Exported modules
	module Database.PostgreSQL.Store.Query.Builder,
	module Database.PostgreSQL.Store.Query.TH,
	module Database.PostgreSQL.Store.Entity,

	-- * Utilities
	castQuery
) where

import Database.PostgreSQL.Store.Types
import Database.PostgreSQL.Store.Entity
import Database.PostgreSQL.Store.Query.Builder
import Database.PostgreSQL.Store.Query.TH

-- | Cast the query's result type.
castQuery :: Query a -> Query b
castQuery (Query s p) =
	Query s p
