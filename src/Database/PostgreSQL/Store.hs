-- |
-- Module:     Database.PostgreSQL.Store
-- Copyright:  (c) Ole Krüger 2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store (
	-- * Errand
	Errand,
	runErrand,

	execute,
	execute',
	query,
	queryWith,

	prepare,

	beginTransaction,
	commitTransaction,
	saveTransaction,
	rollbackTransaction,
	rollbackTransactionTo,
	withTransaction,

	-- * Query
	Query (..),
	PrepQuery (..),
	pgQuery,
	pgPrepQuery,
	pgQueryGen,

	-- * Types
	Oid (..),

	-- * Entity
	Entity (..),

	-- * Tables
	Table (..),
	TableEntity (..),

	-- * Errors
	ErrandError (..),
	ErrorCode (..),
	P.ExecStatus (..),
	RowError (..),
	RowErrorLocation (..),
	RowErrorDetail (..)
) where

import           Database.PostgreSQL.Store.Entity
import           Database.PostgreSQL.Store.Errand
import           Database.PostgreSQL.Store.Query
import           Database.PostgreSQL.Store.RowParser
import           Database.PostgreSQL.Store.Table
import           Database.PostgreSQL.Store.Types

import qualified Database.PostgreSQL.LibPQ           as P
