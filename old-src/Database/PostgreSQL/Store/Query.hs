{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleInstances, UndecidableInstances #-}

-- |
-- Module:     Database.PostgreSQL.Store.Query
-- Copyright:  (c) Ole Krüger 2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.Query (
	-- * Type
	Query (..),
	reinterpretQuery,

	-- * Query Builder
	QueryBuilder,
	buildQuery,

	insertCode,
	insertQuote,
	insertName,
	insertEntity,
	insertTableName,
	insertTableIdentColumnName,
	insertTableColumnNames,

	-- * Template Haskell
	pgsq
) where

import qualified Data.ByteString as B

import           Database.PostgreSQL.Store.Columns
import           Database.PostgreSQL.Store.Query.Builder
import           Database.PostgreSQL.Store.Query.TH

-- | Query including statement and parameters
--
-- The given type variable @r@ hints the returned column types of this query.
data Query r = Query {
	-- | Statement
	queryStatement :: !B.ByteString,

	-- | Parameters
	queryParams :: ![Value]
} deriving (Show, Eq, Ord)

-- | Change the query's return type hint.
reinterpretQuery :: Query a -> Query b
reinterpretQuery (Query s v) =
	Query s v

-- | Wrap query code and values into a 'Query' construct.
instance BuildQuery (Query r) where
	buildQuery builder =
		uncurry Query (buildQuery builder)
