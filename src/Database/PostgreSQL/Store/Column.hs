{-# LANGUAGE OverloadedStrings, FlexibleInstances, ScopedTypeVariables, GeneralizedNewtypeDeriving #-}

-- |
-- Module:     Database.PostgreSQL.Store.ColumnEntity
-- Copyright:  (c) Ole Krüger 2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.Column (
	ColumnType (..),
	ColumnEntity (..)
) where

import           Data.Proxy

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
	describeColumnType :: proxy a -> ColumnType

instance (ColumnEntity a) => ColumnEntity (Maybe a) where
	describeColumnType _ =
		(describeColumnType (Proxy :: Proxy a)) {
			colTypeNotNull = False
		}

newtype PGInt2 = PGInt2 Int16
	deriving (Show, Read, Eq, Ord, Enum, Bounded, Integral, Num, Real)

instance Entity PGInt2 where
	insertEntity (PGInt2 x) =
		insertEntity x

	parseEntity =
		PGInt2 <$> parseEntity

instance ColumnEntity PGInt2 where
	describeColumnType _ =
		ColumnType "int2" True Nothing

newtype PGInt4 = PGInt4 Int32
	deriving (Show, Read, Eq, Ord, Enum, Bounded, Integral, Num, Real)

instance Entity PGInt4 where
	insertEntity (PGInt4 x) =
		insertEntity x

	parseEntity =
		PGInt4 <$> parseEntity

instance ColumnEntity PGInt4 where
	describeColumnType _ =
		ColumnType "int4" True Nothing

newtype PGInt8 = PGInt8 Int64
	deriving (Show, Read, Eq, Ord, Enum, Bounded, Integral, Num, Real)

instance Entity PGInt8 where
	insertEntity (PGInt8 x) =
		insertEntity x

	parseEntity =
		PGInt8 <$> parseEntity

instance ColumnEntity PGInt8 where
	describeColumnType _ =
		ColumnType "int8" True Nothing

-- | Select a type which can contain the given numeric type.
selectBestColumnType :: (Show a, Num a, Ord a, Bounded a) => proxy a -> ColumnType
selectBestColumnType proxy
	| -32768 <= lower && upper <= 32767 =
		ColumnType "int2" True Nothing
	| -2147483648 <= lower && upper <= 2147483647 =
		ColumnType "int4" True Nothing
	| -9223372036854775808 <= lower && upper <= 9223372036854775807 =
		ColumnType "int8" True Nothing
	| otherwise =
		ColumnType (buildByteString ("numeric(" ++ show digits ++ ",0)")) True Nothing
	where
		upper = (const maxBound :: (Bounded a) => proxy a -> a) proxy
		lower = (const minBound :: (Bounded a) => proxy a -> a) proxy
		digits = max (length (show upper)) (length (show lower))

instance ColumnEntity Bool where
	describeColumnType _ =
		ColumnType "bool" True Nothing

instance ColumnEntity Integer where
	describeColumnType _ =
		ColumnType "numeric" True Nothing

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
	describeColumnType _ =
		ColumnType "numeric" True Nothing

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
	describeColumnType _ =
		ColumnType "real" True Nothing

instance ColumnEntity Double where
	describeColumnType _ =
		ColumnType "double precision" True Nothing

instance ColumnEntity Scientific where
	describeColumnType _ =
		ColumnType "numeric" True Nothing

instance ColumnEntity String where
	describeColumnType _ =
		ColumnType "text" True Nothing

instance ColumnEntity T.Text where
	describeColumnType _ =
		ColumnType "text" True Nothing

instance ColumnEntity TL.Text where
	describeColumnType _ =
		ColumnType "text" True Nothing

instance ColumnEntity B.ByteString where
	describeColumnType _ =
		ColumnType "bytea" True Nothing

instance ColumnEntity BL.ByteString where
	describeColumnType _ =
		ColumnType "bytea" True Nothing

instance ColumnEntity A.Value where
	describeColumnType _ =
		ColumnType "json" True Nothing
