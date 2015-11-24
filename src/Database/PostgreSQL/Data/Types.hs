{-# LANGUAGE OverloadedStrings,
             TemplateHaskell,
             ScopedTypeVariables,
             FlexibleInstances,
             ExistentialQuantification #-}

module Database.PostgreSQL.Data.Types where

import           Data.Int
import           Data.Monoid
import qualified Data.ByteString as B

-- | Description of a column type
data ColumnTypeDescription a = ColumnTypeDescription {
	columnTypeIdentifier :: B.ByteString,
	columnTypeNotNull    :: Bool
}

class ColumnType a where
	describeColumnType :: ColumnTypeDescription a

instance ColumnType Int where
	describeColumnType =
		ColumnTypeDescription {
			columnTypeIdentifier = "INTEGER",
			columnTypeNotNull    = True
		}

instance ColumnType Int16 where
	describeColumnType =
		ColumnTypeDescription {
			columnTypeIdentifier = "SMALLINT",
			columnTypeNotNull    = True
		}

instance ColumnType Int32 where
	describeColumnType =
		ColumnTypeDescription {
			columnTypeIdentifier = "INTEGER",
			columnTypeNotNull    = True
		}

instance ColumnType Int64 where
	describeColumnType =
		ColumnTypeDescription {
			columnTypeIdentifier = "BIGINT",
			columnTypeNotNull    = True
		}

instance ColumnType [Char] where
	describeColumnType =
		ColumnTypeDescription {
			columnTypeIdentifier = "VARCHAR(255)",
			columnTypeNotNull    = True
		}

instance ColumnType B.ByteString where
	describeColumnType =
		ColumnTypeDescription {
			columnTypeIdentifier = "BYTEA(255)",
			columnTypeNotNull    = True
		}

instance (ColumnType a) => ColumnType (Maybe a) where
	describeColumnType =
		make describeColumnType
		where
			make :: ColumnTypeDescription a -> ColumnTypeDescription (Maybe a)
			make descr =
				descr {columnTypeNotNull = False}

-- | Type `BIGSERIAL`
newtype BigSerial = BigSerial Int64

instance ColumnType BigSerial where
	describeColumnType =
		ColumnTypeDescription {
			columnTypeIdentifier = "BIGSERIAL",
			columnTypeNotNull    = True
		}

-- | Reference another table type
data Reference a = Reference Int64 | Value a

instance (Table a) => ColumnType (Reference a) where
	describeColumnType =
		make describeTable
		where
			make :: TableDescription a -> ColumnTypeDescription (Reference a)
			make descr =
				ColumnTypeDescription {
					columnTypeIdentifier = "BIGINT REFERENCES \"" <> tableName descr <> "\" (id)",
					columnTypeNotNull = True
				}

-- | Description of a column
data ColumnDescription = forall a. ColumnDescription {
	columnName :: B.ByteString,
	columnType :: ColumnTypeDescription a
}

-- | Description of a table
data TableDescription a = TableDescription {
	tableName    :: B.ByteString,
	tableColumns :: [ColumnDescription]
}

class Table a where
	describeTable :: TableDescription a
