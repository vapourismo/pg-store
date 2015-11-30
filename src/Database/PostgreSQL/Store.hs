-- |
-- Module     : Database.PostgreSQL.Store
-- Copyright  : (c) Ole Krüger 2015
-- License    : BSD3
-- Maintainer : Ole Krüger <ole@vprsm.de>
--
-- Further description here.
module Database.PostgreSQL.Store (
	-- * Tables
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
	update,
	delete
) where

import Database.PostgreSQL.Store.Table
import Database.PostgreSQL.Store.Query
import Database.PostgreSQL.Store.Errand
import Database.PostgreSQL.Store.Result
