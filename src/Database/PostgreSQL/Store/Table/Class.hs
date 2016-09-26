{-# LANGUAGE TemplateHaskell, BangPatterns, DeriveLift #-}

-- |
-- Module:     Database.PostgreSQL.Store.Table.Class
-- Copyright:  (c) Ole Krüger 2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.Table.Class (
	TableInformation (..),
	Table (..)
) where

import qualified Data.ByteString as B

import           Database.PostgreSQL.Store.Columns

-- | Table-related information about a type.
data TableInformation = TableInformation {
	tableName        :: !B.ByteString,
	tableIdentColumn :: !B.ByteString,
	tableColumns     :: ![(B.ByteString, ColumnInformation)]
}

-- | Attach table information to a type.
class Table a where
	tableInfo :: proxy a -> TableInformation

	unpackRow :: a -> [Value]
