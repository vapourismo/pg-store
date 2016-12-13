{-# LANGUAGE OverloadedStrings, FlexibleInstances, ScopedTypeVariables #-}

-- |
-- Module:     Database.PostgreSQL.Store.ColumnEntity
-- Copyright:  (c) Ole Krüger 2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.ColumnEntity (
	ColumnType (..),
	ColumnEntity (..)
) where

import           Data.Proxy
import qualified Data.ByteString as B

import           Database.PostgreSQL.Store.Entity
import           Database.PostgreSQL.Store.Query.Builder

-- | Description of a column type
data ColumnType = ColumnType {
	-- | Type name
	colTypeName :: B.ByteString,

	-- | @NOT NULL@ constraint present?
	colTypeNotNull :: Bool,

	-- | Produce a check statement body when given a column name
	colTypeCheck :: Maybe (B.ByteString -> QueryBuilder)
}

-- | Classify a type which can be used as a column in a table.
class (Entity a) => ColumnEntity a where
	-- | Describe the column type
	describeColumnType :: proxy a -> ColumnType

instance (ColumnEntity a) => ColumnEntity (Maybe a) where
	describeColumnType _ =
		(describeColumnType (Proxy :: Proxy a)) {
			colTypeNotNull = False
		}

instance ColumnEntity Int where
	describeColumnType _ =
		ColumnType "int8" True Nothing

instance ColumnEntity String where
	describeColumnType _ =
		ColumnType "text" True Nothing
