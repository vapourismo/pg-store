module Database.PostgreSQL.Data.SQL (
	generateTableSchema
) where

import Data.List
import Database.PostgreSQL.Data.Types

-- | Generate the SQL segment for a column type.
generateColumnTypeSchema :: ColumnTypeDescription -> String
generateColumnTypeSchema descr =
	columnTypeIdentifier descr ++
	if columnTypeNotNull descr then " NOT NULL" else ""

-- | Generate the SQL segment for a column.
generateColumnSchema :: ColumnDescription -> String
generateColumnSchema descr =
	"\"" ++ columnName descr ++ "\" " ++ generateColumnTypeSchema (columnType descr)

-- | Generate a SQL statement which creates a table.
generateTableSchema :: TableDescription -> String
generateTableSchema descr =
	"CREATE TABLE \"" ++ tableName descr ++ "\" (" ++
		"id BIGSERIAL NOT NULL PRIMARY KEY, " ++
		intercalate ", " (map generateColumnSchema (tableColumns descr)) ++
	")"

