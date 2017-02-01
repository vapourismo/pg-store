-- |
-- Module:     Database.PostgreSQL.Store.Types
-- Copyright:  (c) Ole Krüger 2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.Types (
	-- * General
	Value (..),

	TypedValue (..),
	nullValue,
	placeholderValue,
	anyTypeValue,

	Query (..),
	PrepQuery (..),

	Value2 (..),
	Query2 (..),
	PrepQuery2 (..)
) where

import           Text.Show.Functions ()

import qualified Data.ByteString as B

import           Database.PostgreSQL.LibPQ (Oid (..), invalidOid)

-- | Value of a cell in the result set
newtype Value = Value { valueData :: B.ByteString }
	deriving (Show, Eq, Ord)

-- | Value and type 'Oid' of a cell in the result set
data TypedValue = TypedValue Oid (Maybe Value)
	deriving (Show, Eq, Ord)

-- | NULL value
nullValue :: TypedValue
nullValue = TypedValue invalidOid Nothing

-- | Placeholder value
placeholderValue :: TypedValue
placeholderValue = TypedValue (Oid maxBound) Nothing

-- | Value with any type
anyTypeValue :: Value -> TypedValue
anyTypeValue value = TypedValue invalidOid (Just value)

-- | Query
data Query a = Query {
	queryStatement :: B.ByteString,
	queryParams    :: [TypedValue]
} deriving (Show, Eq, Ord)

-- | Preparable query
data PrepQuery a = PrepQuery {
	prepQueryName :: B.ByteString,
	prepQuery     :: Query a
} deriving (Show, Eq, Ord)

-- | Replacement for 'TypedValue'
data Value2
	= Value2 Oid B.ByteString
	| Null2
	deriving (Show, Eq, Ord)

-- | Replacement for 'Query'
data Query2 = Query2 {
	query2Statement :: B.ByteString,
	query2Params    :: [Value2]
} deriving (Show, Eq, Ord)

-- | Replacement for 'PrepQuery'
data PrepQuery2 a = PrepQuery2 {
	prep2Name      :: B.ByteString,
	prep2Statement :: B.ByteString,
	prep2Params    :: a -> [Value2]
} deriving (Show)
