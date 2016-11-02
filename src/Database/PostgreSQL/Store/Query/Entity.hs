{-# LANGUAGE OverloadedStrings, FlexibleInstances, TypeOperators, DataKinds, ScopedTypeVariables,
             ConstraintKinds, TypeFamilies, DefaultSignatures, UndecidableInstances #-}

-- |
-- Module:     Database.PostgreSQL.Store.Query.Entity
-- Copyright:  (c) Ole Krüger 2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.Query.Entity (
	-- * Query Entity
	QueryEntity (..),

	-- * Generic Builder
	GenericQueryEntity,
	insertGeneric,

	-- * Helpers
	GQuerySel (..),
	GQueryEnum (..),
	GQueryCons (..)
) where

import           GHC.Generics
import           GHC.TypeLits
import           Data.Proxy

import           Data.Int
import           Data.Word
import           Numeric
import           Numeric.Natural

import qualified Data.Aeson              as A

import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy    as BL

import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import qualified Data.Text.Lazy          as TL

import           Database.PostgreSQL.LibPQ (Oid (..), invalidOid)

import           Database.PostgreSQL.Store.Types
import           Database.PostgreSQL.Store.Utilities
import           Database.PostgreSQL.Store.Query.Builder

-- | @sel@ represents the selectors of a constructor.
class GQuerySel sel where
	insertSel :: sel x -> QueryBuilder

-- | Single selector
instance (QueryEntity a) => GQuerySel (S1 meta (Rec0 a)) where
	insertSel (M1 (K1 x)) = insertEntity x

-- | Multiple selectors
instance (GQuerySel lhs, GQuerySel rhs) => GQuerySel (lhs :*: rhs) where
	insertSel (lhs :*: rhs) = do
		insertSel lhs
		insertCode ","
		insertSel rhs

-- | @cons@ represents the constructors of a data type.
class GQueryCons cons where
	insertCons :: cons x -> QueryBuilder

-- | Single constructor
instance (GQuerySel sel) => GQueryCons (C1 meta sel) where
	insertCons (M1 sel) = insertSel sel

-- | Multiple constructors; each constructor must qualify as an enum value and can't have any
--   fields.
instance (GQueryEnum lhs, GQueryEnum rhs) => GQueryCons (lhs :+: rhs) where
	insertCons (L1 lhs) = insertQuote (enumValue lhs)
	insertCons (R1 rhs) = insertQuote (enumValue rhs)

-- | @enum@ represents the constructors without selectors.
class GQueryEnum enum where
	enumValue :: enum x -> B.ByteString

-- | Single constructor
instance (KnownSymbol name) => GQueryEnum (C1 ('MetaCons name meta1 meta2) U1) where
	enumValue _ =
		buildByteString (symbolVal (Proxy :: Proxy name))

-- | Multiple constructors
instance (GQueryEnum lhs, GQueryEnum rhs) => GQueryEnum (lhs :+: rhs) where
	enumValue (L1 lhs) = enumValue lhs
	enumValue (R1 rhs) = enumValue rhs

-- | Constrain @a@ to be a data type that satisfies one of the following properties:
--
--   * single constructor with 1 or more fields
--   * multiple constructors with no fields
--
type GenericQueryEntity meta cons a = (Generic a, Rep a ~ D1 meta cons, GQueryCons cons)

-- | Insert a generic data type.
insertGeneric :: (GenericQueryEntity meta cons a) => a -> QueryBuilder
insertGeneric x = insertCons (unM1 (from x))

-- | An entity that can be inserted into the query.
class QueryEntity a where
	-- | Insert @a@ into the query.
	insertEntity :: a -> QueryBuilder

	default insertEntity :: (GenericQueryEntity meta cons a) => a -> QueryBuilder
	insertEntity = insertGeneric

-- | Generic instance
instance {-# OVERLAPPABLE #-} (GenericQueryEntity meta cons a) => QueryEntity a where
	insertEntity = insertGeneric

-- | 2 query entities in sequence
instance (QueryEntity a, QueryEntity b) => QueryEntity (a, b)

-- | 3 query entities in sequence
instance (QueryEntity a, QueryEntity b, QueryEntity c) => QueryEntity (a, b, c)

-- | 4 query entities in sequence
instance (QueryEntity a, QueryEntity b, QueryEntity c, QueryEntity d) => QueryEntity (a, b, c, d)

-- | 5 query entities in sequence
instance (QueryEntity a, QueryEntity b, QueryEntity c, QueryEntity d, QueryEntity e)
         => QueryEntity (a, b, c, d, e)

-- | 6 query entities in sequence
instance (QueryEntity a, QueryEntity b, QueryEntity c, QueryEntity d, QueryEntity e, QueryEntity f)
         => QueryEntity (a, b, c, d, e, f)

-- | 7 query entities in sequence
instance (QueryEntity a, QueryEntity b, QueryEntity c, QueryEntity d, QueryEntity e, QueryEntity f,
          QueryEntity g)
         => QueryEntity (a, b, c, d, e, f, g)

-- | Untyped value
instance QueryEntity Value where
	insertEntity = insertValue

-- | Typed value
instance QueryEntity TypedValue where
	insertEntity = insertTypedValue

-- | Underlying value or @NULL@
instance (QueryEntity a) => QueryEntity (Maybe a) where
	insertEntity Nothing  = insertTypedValue (TypedValue invalidOid Nothing)
	insertEntity (Just x) = insertEntity x

-- | @boolean@ - @t@ for 'True' and @f@ for 'False'
instance QueryEntity Bool where
	insertEntity input =
		insertTypedValue (TypedValue (Oid 16) (Just (Value value)))
		where value | input = "t" | otherwise = "f"

-- | Insert a numeric value.
insertNumericValue :: (Show a) => a -> QueryBuilder
insertNumericValue x =
	insertTypedValue (TypedValue (Oid 1700) (Just (Value (showByteString x))))

-- | Any numeric
instance QueryEntity Integer where
	insertEntity = insertNumericValue

-- | Any numeric
instance QueryEntity Int where
	insertEntity = insertNumericValue

-- | Any numeric
instance QueryEntity Int8 where
	insertEntity = insertNumericValue

-- | Any numeric
instance QueryEntity Int16 where
	insertEntity = insertNumericValue

-- | Any numeric
instance QueryEntity Int32 where
	insertEntity = insertNumericValue

-- | Any numeric
instance QueryEntity Int64 where
	insertEntity = insertNumericValue

-- | Any numeric
instance QueryEntity Natural where
	insertEntity = insertNumericValue

-- | Any numeric
instance QueryEntity Word where
	insertEntity = insertNumericValue

-- | Any numeric
instance QueryEntity Word8 where
	insertEntity = insertNumericValue

-- | Any numeric
instance QueryEntity Word16 where
	insertEntity = insertNumericValue

-- | Any numeric
instance QueryEntity Word32 where
	insertEntity = insertNumericValue

-- | Any numeric
instance QueryEntity Word64 where
	insertEntity = insertNumericValue

-- | @char@, @varchar@ or @text@ - UTF-8 encoded
instance QueryEntity String where
	insertEntity value =
		insertTypedValue (TypedValue (Oid 25) (Just (Value (buildByteString value))))

-- | @char@, @varchar@ or @text@ - UTF-8 encoded
instance QueryEntity T.Text where
	insertEntity value =
		insertTypedValue (TypedValue (Oid 25) (Just (Value (T.encodeUtf8 value))))

-- | @char@, @varchar@ or @text@ - UTF-8 encoded
instance QueryEntity TL.Text where
	insertEntity value =
		insertEntity (TL.toStrict value)

-- | @bytea@ - byte array encoded in hex format
instance QueryEntity B.ByteString where
	insertEntity value =
		insertTypedValue (TypedValue (Oid 17) (Just (Value dat)))
		where
			dat = B.append "\\x" (B.concatMap showHex' value)

			showHex' n =
				buildByteString $ case showHex n [] of
					(a : b : _) -> [a, b]
					(a : _)     -> ['0', a]
					[]          -> "00"

-- | @bytea@ - byte array encoded in hex format
instance QueryEntity BL.ByteString where
	insertEntity value =
		insertEntity (BL.toStrict value)

-- | @json@ or @jsonb@
instance QueryEntity A.Value where
	insertEntity value =
		insertTypedValue (TypedValue (Oid 114) (Just (Value (BL.toStrict (A.encode value)))))
