{-# LANGUAGE TemplateHaskell #-}

module Database.PostgreSQL.Data where

import Data.List
import Database.PostgreSQL.Data.TH
import Database.PostgreSQL.Data.Types

generateColumnTypeSchema :: ColumnTypeDescription -> String
generateColumnTypeSchema descr =
	columnTypeIdentifier descr ++
	if columnTypeNotNull descr then " NOT NULL" else ""

generateColumnSchema :: ColumnDescription -> String
generateColumnSchema descr =
	"\"" ++ columnName descr ++ "\" " ++ generateColumnTypeSchema (columnType descr)

generateTableSchema :: TableDescription -> String
generateTableSchema descr =
	"CREATE TABLE \"" ++ tableName descr ++ "\" (" ++
		intercalate ", " (map generateColumnSchema (tableColumns descr)) ++
	")"

data MyTable = MyTable {
	mtID       :: BigSerial,
	mtName     :: String,
	mtLastName :: Maybe String
}

makeTable ''MyTable

myTableDescription :: TableDescription
myTableDescription = describeTable (undefined :: proxy MyTable)
