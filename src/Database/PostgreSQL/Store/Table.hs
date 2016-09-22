{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module:     Database.PostgreSQL.Store.Table
-- Copyright:  (c) Ole Krüger 2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.Table (
	-- * Table information
	TableInformation (..),
	Table (..),

	-- * Helpers
	makeTable
) where

import Language.Haskell.TH

import Database.PostgreSQL.Store.Table.Class
import Database.PostgreSQL.Store.Table.TH

-- | Implement 'Table' for a type.
makeTable :: Name -> Q [Dec]
makeTable typeName =
	checkTableName typeName >>= implementTable
