-- |
-- Module:     Database.PostgreSQL.Store
-- Copyright:  (c) Ole Krüger 2015-2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store (
	-- * Tables
	TableConstraint (..),
	mkTable,
	Row (..),
	Reference (..),

	-- * Queries
	Query (..),
	pgsq,
	mkCreateQuery,

	-- * Errands
	ResultError (..),
	ErrandError (..),
	Errand,
	runErrand,
	query,
	query_,
	insert,
	find,
	update,
	delete
) where

import Database.PostgreSQL.Store.Table
import Database.PostgreSQL.Store.Query
import Database.PostgreSQL.Store.Errand
import Database.PostgreSQL.Store.Result
