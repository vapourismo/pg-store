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

instance ColumnType Int where
	describeColumnType _ =
		ColumnTypeDescription {
			columnTypeIdentifier = "INTEGER",
			columnTypeNotNull    = True
		}

instance ColumnType Int16 where
	describeColumnType _ =
		ColumnTypeDescription {
			columnTypeIdentifier = "SMALLINT",
			columnTypeNotNull    = True
		}

instance ColumnType Int32 where
	describeColumnType _ =
		ColumnTypeDescription {
			columnTypeIdentifier = "INTEGER",
			columnTypeNotNull    = True
		}

instance ColumnType Int64 where
	describeColumnType _ =
		ColumnTypeDescription {
			columnTypeIdentifier = "BIGINT",
			columnTypeNotNull    = True
		}

instance ColumnType [Char] where
	describeColumnType _ =
		ColumnTypeDescription {
			columnTypeIdentifier = "VARCHAR(255)",
			columnTypeNotNull    = True
		}

instance (ColumnType a) => ColumnType (Maybe a) where
	describeColumnType proxy =
		(describeColumnType (unMaybe proxy)) {
			columnTypeNotNull = False
		}
		where
			unMaybe :: proxy (Maybe a) -> proxy a
			unMaybe _ = undefined

-- | Type `BIGSERIAL`
newtype BigSerial = BigSerial Int64

instance ColumnType BigSerial where
	describeColumnType _ =
		ColumnTypeDescription {
			columnTypeIdentifier = "BIGSERIAL",
			columnTypeNotNull    = True
		}

-- | Reference another table type
data Reference a = Reference Int64 | Value a
	deriving (Show, Eq, Ord)

instance (Table a) => ColumnType (Reference a) where
	describeColumnType proxy =
		ColumnTypeDescription {
			columnTypeIdentifier = "BIGINT REFERENCES \"" ++ tableName refDescr ++ "\" (id)",
			columnTypeNotNull    = True
		}
		where
			unReference :: proxy (Reference a) -> proxy a
			unReference _ = undefined

			refDescr = describeTable (unReference proxy)

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
