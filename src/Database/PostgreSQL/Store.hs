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

	insert,
	insertMany,
	deleteAll,
	findAll,
	create,

	-- * Query
	Query (..),
	pgsq,
	castQuery,

	-- * Entity
	Entity (..),

	-- * Tables
	TableEntity (..),
	ColumnEntity (..),

	Table (..),
	ColumnType (..),
	Column (..),

	-- * Errors
	ErrandError (..),
	ErrorCode (..),
	P.ExecStatus,
	RowError (..),
	RowErrorLocation (..),
	RowErrorDetail (..)
) where

import           Database.PostgreSQL.Store.Query
import           Database.PostgreSQL.Store.Entity
import           Database.PostgreSQL.Store.Table
import           Database.PostgreSQL.Store.Errand

import qualified Database.PostgreSQL.LibPQ as P
