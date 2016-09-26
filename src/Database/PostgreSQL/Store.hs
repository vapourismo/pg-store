-- |
-- Module:     Database.PostgreSQL.Store
-- Copyright:  (c) Ole Krüger 2015-2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store (
	-- * Errands
	Errand,
	ErrandError (..),
	ErrorCode (..),
	ExecStatus (..),
	runErrand,

	execute,
	query,
	query_,
	queryWith,

	Reference (..),

	insert,
	insertMany,

	-- * Queries
	Query (..),
	pgsq,

	-- * Values
	Value (..),
	Column (..),

	-- * Results
	Result (..),
	ResultProcessor,
	ResultError (..),
	skipColumn,
	unpackColumn,

	-- * Tables
	TableInformation (..),
	Table (..),

	TableOptions (..),
	defaultTableOptions,
	makeTable
) where

import Database.PostgreSQL.Store.Columns
import Database.PostgreSQL.Store.Errand
import Database.PostgreSQL.Store.Query
import Database.PostgreSQL.Store.Result
import Database.PostgreSQL.Store.Table
