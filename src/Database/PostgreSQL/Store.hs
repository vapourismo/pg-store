module Database.PostgreSQL.Store (
	mkTable,
	mkCreateQuery,
	pgsq
) where

import Database.PostgreSQL.Store.TH.Table
import Database.PostgreSQL.Store.TH.Query
