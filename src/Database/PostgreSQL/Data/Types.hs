{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, FlexibleInstances #-}

module Database.PostgreSQL.Data.Types where

import Data.Int

-- | Description of a column type
data ColumnTypeDescription = ColumnTypeDescription {
	columnTypeIdentifier :: String,
	columnTypeNotNull    :: Bool
} deriving (Show, Eq, Ord)

class ColumnType a where
	describeColumnType :: proxy a -> ColumnTypeDescription

-- | Type `BIGSERIAL`
newtype BigSerial = BigSerial Int64

instance ColumnType Int where
	describeColumnType _ =
		ColumnTypeDescription {
			columnTypeIdentifier = "INTEGER",
			columnTypeNotNull    = True
		}

instance ColumnType [Char] where
	describeColumnType _ =
		ColumnTypeDescription {
			columnTypeIdentifier = "VARCHAR(255)",
			columnTypeNotNull    = True
		}

instance ColumnType BigSerial where
	describeColumnType _ =
		ColumnTypeDescription {
			columnTypeIdentifier = "BIGSERIAL",
			columnTypeNotNull    = True
		}

instance (ColumnType a) => ColumnType (Maybe a) where
	describeColumnType x =
		(describeColumnType (unMaybe x)) {
			columnTypeNotNull = False
		}
		where
			unMaybe :: proxy (Maybe a) -> proxy a
			unMaybe _ = undefined

-- | Description of a column
data ColumnDescription = ColumnDescription {
	columnName :: String,
	columnType :: ColumnTypeDescription
} deriving (Show, Eq, Ord)

-- | Description of a table
data TableDescription = TableDescription {
	tableName :: String,
	tableColumns :: [ColumnDescription]
} deriving (Show, Eq, Ord)

class Table a where
	describeTable :: proxy a -> TableDescription
