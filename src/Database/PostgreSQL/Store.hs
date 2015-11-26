module Database.PostgreSQL.Store (
	mkTable,
	mkCreateQuery,
	mkDropQuery,
	pgsq
) where

import Database.PostgreSQL.Store.TH.Table
import Database.PostgreSQL.Store.TH.Query
