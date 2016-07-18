-- |
-- Module:     Database.PostgreSQL.Store
-- Copyright:  (c) Ole Krüger 2015-2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store (
	-- * Errands
	Errand,
	ErrandError (..),
	runErrand,
	execute,
	query,
	query_,
	queryWith,

	-- * Queries
	Query (..),
	pgsq,
	pgss,

	QueryTable (..),
	SelectorElement (..),

	-- * Values
	Value (..),
	Column (..),

	-- * Results
	Result (..),
	ResultProcessor,
	ResultError (..),
	skipColumn,
	unpackColumn,

	Single (..),
	Reference (..),

	-- * Tables
	Table (..),
	mkCreateQuery,

	mkTable,
	TableConstraint (..)
) where

import Database.PostgreSQL.Store.Columns
import Database.PostgreSQL.Store.Errand
import Database.PostgreSQL.Store.Query
import Database.PostgreSQL.Store.Result
import Database.PostgreSQL.Store.Table
