{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Database.PostgreSQL.Data.SQL (
	generateTableSchema,
	generateInsertStatement
) where

import           Data.Monoid
import qualified Data.ByteString as B
import           Database.PostgreSQL.Data.Types

-- | Generate the SQL segment for a column type.
generateColumnTypeSchema :: ColumnTypeDescription a -> B.ByteString
generateColumnTypeSchema ColumnTypeDescription {..} =
	columnTypeIdentifier <> if columnTypeNotNull then " NOT NULL" else ""

-- | Generate the SQL segment for a column.
generateColumnSchema :: ColumnDescription -> B.ByteString
generateColumnSchema ColumnDescription {..} =
	"\"" <> columnName <> "\" " <> generateColumnTypeSchema columnType

-- | Generate a SQL statement which creates a table.
generateTableSchema :: TableDescription a -> B.ByteString
generateTableSchema TableDescription {..} =
	"CREATE TABLE \"" <> tableName <> "\" (" <>
		"id BIGSERIAL NOT NULL PRIMARY KEY, " <>
		B.intercalate ", " (map generateColumnSchema tableColumns) <>
	")"

-- | Generate a SQL statement which inserts a row.
generateInsertStatement :: TableDescription a -> B.ByteString
generateInsertStatement TableDescription {..} =
	"INSERT INTO \"" <> tableName <> "\" (" <> B.intercalate ", " keys <> ") " <>
	"VALUES (" <> B.intercalate ", " values <> ") RETURNING id"
	where
		keys = map (\ col -> "\"" <> columnName col <> "\"") tableColumns
		values = replicate (length tableColumns) "?"
