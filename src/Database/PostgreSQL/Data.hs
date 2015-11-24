{-# LANGUAGE TemplateHaskell #-}

module Database.PostgreSQL.Data where

import Data.Int

import Database.PostgreSQL.Data.TH
import Database.PostgreSQL.Data.SQL
import Database.PostgreSQL.Data.Types

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types

insertInto :: (Table a) => Connection -> a -> IO (Maybe (Reference a))
insertInto con row =
	extract <$> make describeTable row
	where
		make :: (ToRow a) => TableDescription a -> a -> IO [Only Int64]
		make desc row =
			let q = generateInsertStatement desc
			in query con (Query q) row

		extract [] = Nothing
		extract (Only rid : _) = Just (Value rid row)

createTable :: Connection -> TableDescription a -> IO Int64
createTable con desc =
	execute_ con (Query (generateTableSchema desc))

data AnotherTable = AnotherTable {
	atValue :: Int
} deriving (Show, Eq, Ord)

data MyTable = MyTable {
	mtName  :: String,
	mtOther :: Reference AnotherTable
} deriving (Show, Eq, Ord)

makeTable ''AnotherTable
makeTable ''MyTable

anotherTableDescription :: TableDescription AnotherTable
anotherTableDescription = describeTable

myTableDescription :: TableDescription MyTable
myTableDescription = describeTable
