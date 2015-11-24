{-# LANGUAGE RecordWildCards #-}

module Database.PostgreSQL.Data.SQL (
	generateTableSchema
) where

import Data.List
import Database.PostgreSQL.Data.Types

-- | Generate the SQL segment for a column type.
generateColumnTypeSchema :: ColumnTypeDescription a -> String
generateColumnTypeSchema ColumnTypeDescription {..} =
	columnTypeIdentifier ++ if columnTypeNotNull then " NOT NULL" else ""

-- | Generate the SQL segment for a column.
generateColumnSchema :: ColumnDescription -> String
generateColumnSchema ColumnDescription {..} =
	"\"" ++ columnName ++ "\" " ++ generateColumnTypeSchema columnType

-- | Generate a SQL statement which creates a table.
generateTableSchema :: TableDescription -> String
generateTableSchema TableDescription {..} =
	"CREATE TABLE \"" ++ tableName ++ "\" (" ++
		"id BIGSERIAL NOT NULL PRIMARY KEY, " ++
		intercalate ", " (map generateColumnSchema tableColumns) ++
	")"

