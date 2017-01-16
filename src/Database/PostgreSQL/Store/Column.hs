{-# LANGUAGE OverloadedStrings,
             FlexibleInstances,
             ScopedTypeVariables,
             GeneralizedNewtypeDeriving,
             TypeApplications #-}

-- |
-- Module:     Database.PostgreSQL.Store.ColumnEntity
-- Copyright:  (c) Ole Krüger 2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.Column (
	ColumnType (..),
	ColumnEntity (..)
) where

import           Data.Tagged

import           Data.Int
import           Data.Word
import           Data.Scientific
import           Numeric.Natural

import qualified Data.Aeson           as A

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as TL

import           Database.PostgreSQL.Store.Entity
import           Database.PostgreSQL.Store.Utilities
import           Database.PostgreSQL.Store.Query.Builder

-- | Description of a column type
data ColumnType = ColumnType {
	-- | Type name
	colTypeName :: B.ByteString,

	-- | @NOT NULL@ constraint present?
	colTypeNotNull :: Bool,

	-- | Produce a check statement body when given a column name
	colTypeCheck :: Maybe (B.ByteString -> QueryBuilder)
}

-- | Classify a type which can be used as a column in a table.
class (Entity a) => ColumnEntity a where
	-- | Describe the column type
	describeColumnType :: Tagged a ColumnType

instance (ColumnEntity a) => ColumnEntity (Maybe a) where
	describeColumnType =
		Tagged ((untag (describeColumnType @a)) {colTypeNotNull = False})

newtype PGInt2 = PGInt2 Int16
	deriving (Show, Read, Eq, Ord, Enum, Bounded, Integral, Num, Real)

instance Entity PGInt2 where
	insertEntity (PGInt2 x) =
		insertEntity x

	parseEntity =
		PGInt2 <$> parseEntity

instance ColumnEntity PGInt2 where
	describeColumnType =
		Tagged (ColumnType "int2" True Nothing)

newtype PGInt4 = PGInt4 Int32
	deriving (Show, Read, Eq, Ord, Enum, Bounded, Integral, Num, Real)

instance Entity PGInt4 where
	insertEntity (PGInt4 x) =
		insertEntity x

	parseEntity =
		PGInt4 <$> parseEntity

instance ColumnEntity PGInt4 where
	describeColumnType =
		Tagged (ColumnType "int4" True Nothing)

newtype PGInt8 = PGInt8 Int64
	deriving (Show, Read, Eq, Ord, Enum, Bounded, Integral, Num, Real)

instance Entity PGInt8 where
	insertEntity (PGInt8 x) =
		insertEntity x

	parseEntity =
		PGInt8 <$> parseEntity

instance ColumnEntity PGInt8 where
	describeColumnType =
		Tagged (ColumnType "int8" True Nothing)

-- | Select a type which can contain the given numeric type.
selectBestColumnType :: forall a. (Show a, Num a, Ord a, Bounded a) => Tagged a ColumnType
selectBestColumnType
	| -32768 <= lower && upper <= 32767 =
		Tagged (ColumnType "int2" True Nothing)
	| -2147483648 <= lower && upper <= 2147483647 =
		Tagged (ColumnType "int4" True Nothing)
	| -9223372036854775808 <= lower && upper <= 9223372036854775807 =
		Tagged (ColumnType "int8" True Nothing)
	| otherwise =
		Tagged (ColumnType (buildByteString ("numeric(" ++ show digits ++ ",0)")) True Nothing)
	where
		upper = maxBound :: a
		lower = minBound :: a
		digits = max (length (show upper)) (length (show lower))

instance ColumnEntity Bool where
	describeColumnType =
		Tagged (ColumnType "bool" True Nothing)

instance ColumnEntity Integer where
	describeColumnType =
		Tagged (ColumnType "numeric" True Nothing)

instance ColumnEntity Int where
	describeColumnType = selectBestColumnType

instance ColumnEntity Int8 where
	describeColumnType = selectBestColumnType

instance ColumnEntity Int16 where
	describeColumnType = selectBestColumnType

instance ColumnEntity Int32 where
	describeColumnType = selectBestColumnType

instance ColumnEntity Int64 where
	describeColumnType = selectBestColumnType

instance ColumnEntity Natural where
	describeColumnType =
		Tagged (ColumnType "numeric" True Nothing)

instance ColumnEntity Word where
	describeColumnType = selectBestColumnType

instance ColumnEntity Word8 where
	describeColumnType = selectBestColumnType

instance ColumnEntity Word16 where
	describeColumnType = selectBestColumnType

instance ColumnEntity Word32 where
	describeColumnType = selectBestColumnType

instance ColumnEntity Word64 where
	describeColumnType = selectBestColumnType

instance ColumnEntity Float where
	describeColumnType =
		Tagged (ColumnType "real" True Nothing)

instance ColumnEntity Double where
	describeColumnType =
		Tagged (ColumnType "double precision" True Nothing)

instance ColumnEntity Scientific where
	describeColumnType =
		Tagged (ColumnType "numeric" True Nothing)

instance ColumnEntity String where
	describeColumnType =
		Tagged (ColumnType "text" True Nothing)

instance ColumnEntity T.Text where
	describeColumnType =
		Tagged (ColumnType "text" True Nothing)

instance ColumnEntity TL.Text where
	describeColumnType =
		Tagged (ColumnType "text" True Nothing)

instance ColumnEntity B.ByteString where
	describeColumnType =
		Tagged (ColumnType "bytea" True Nothing)

instance ColumnEntity BL.ByteString where
	describeColumnType =
		Tagged (ColumnType "bytea" True Nothing)

instance ColumnEntity A.Value where
	describeColumnType =
		Tagged (ColumnType "json" True Nothing)
