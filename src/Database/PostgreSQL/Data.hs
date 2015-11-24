{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Database.PostgreSQL.Data where

import Database.PostgreSQL.Data.TH
import Database.PostgreSQL.Data.SQL
import Database.PostgreSQL.Data.Types

data AnotherTable = AnotherTable {
	atValue :: Int
}

data MyTable = MyTable {
	mtName  :: String,
	mtOther :: Reference AnotherTable
}

makeTable ''AnotherTable
makeTable ''MyTable

anotherTableDescription :: TableDescription AnotherTable
anotherTableDescription = describeTable

myTableDescription :: TableDescription MyTable
myTableDescription = describeTable
