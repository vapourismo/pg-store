{-# LANGUAGE TemplateHaskell #-}

module Database.PostgreSQL.Data where

import Database.PostgreSQL.Data.TH
import Database.PostgreSQL.Data.SQL
import Database.PostgreSQL.Data.Types

data MyTable = MyTable {
	mtName  :: String,
	mtOther :: Int
}

makeTable ''MyTable

myTableDescription :: TableDescription MyTable
myTableDescription = describeTable
