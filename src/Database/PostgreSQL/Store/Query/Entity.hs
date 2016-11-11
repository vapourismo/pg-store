{-# LANGUAGE OverloadedStrings,
             ConstraintKinds,
             DataKinds,
             DefaultSignatures,
             FlexibleContexts,
             FlexibleInstances,
             ScopedTypeVariables,
             TypeFamilies,
             TypeOperators,
             UndecidableInstances
#-}

-- |
-- Module:     Database.PostgreSQL.Store.Query.Entity
-- Copyright:  (c) Ole Krüger 2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.Query.Entity (
	-- * Query Entity
	QueryEntity (..),

	-- * Generic Builder
	insertGeneric,

	-- * Helpers
	GQueryRecord (..),
	GQueryEnum (..),
	GQueryEntity (..),
) where

import           GHC.Generics
import           GHC.TypeLits
import           Data.Proxy

import           Data.Int
import           Data.Word
import           Data.Scientific
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
import           Database.PostgreSQL.Store.GenericEntity
import           Database.PostgreSQL.Store.Query.Builder

-- | Insert methods for records
class GQueryRecord (rec :: KRecord) where
	gInsertRecord :: Record rec -> QueryBuilder

instance (QueryEntity typ) => GQueryRecord ('TSingle meta typ) where
	gInsertRecord (Single x) = insertEntity x

instance (GQueryRecord lhs, GQueryRecord rhs) => GQueryRecord ('TCombine lhs rhs) where
	gInsertRecord (Combine lhs rhs) = do
		gInsertRecord lhs
		insertCode ","
		gInsertRecord rhs

-- | Insert methods for enumerations
class GQueryEnum (enum :: KFlatSum) where
	gInsertEnum :: FlatSum enum -> QueryBuilder

instance (KnownSymbol name) => GQueryEnum ('TValue ('MetaCons name f r)) where
	gInsertEnum Unit =
		insertQuote (buildByteString (symbolVal (Proxy :: Proxy name)))

instance (GQueryEnum lhs, GQueryEnum rhs) => GQueryEnum ('TChoose lhs rhs) where
	gInsertEnum (ChooseLeft lhs)  = gInsertEnum lhs
	gInsertEnum (ChooseRight rhs) = gInsertEnum rhs

-- | Insert methods for generic entities
class GQueryEntity (dat :: KDataType) where
	gInsertEntity :: DataType dat -> QueryBuilder

instance (GQueryRecord rec) => GQueryEntity ('TRecord d c rec) where
	gInsertEntity (Record x) = gInsertRecord x

instance (GQueryEnum enum) => GQueryEntity ('TFlatSum d enum) where
	gInsertEntity (FlatSum x) = gInsertEnum x

-- | Insert generic entity into the query.
insertGeneric :: (GenericEntity a, GQueryEntity (AnalyzeEntity a)) => a -> QueryBuilder
insertGeneric x = gInsertEntity (fromGenericEntity x)

-- | An entity that can be inserted into the query.
--
-- If @a@ is a data type that matches one of the following criteria, then you may provide an empty
-- instance or skip defining the instance.
--
--  * single constructor with 1 or more records
--  * multiple constructors with no records
--
class QueryEntity a where
	-- | Insert @a@ into the query.
	insertEntity :: a -> QueryBuilder

	default insertEntity :: (GenericEntity a, GQueryEntity (AnalyzeEntity a)) => a -> QueryBuilder
	insertEntity = insertGeneric

-- | Generic instance - See 'QueryEntity' documentation
instance {-# OVERLAPPABLE #-} (GenericEntity a, GQueryEntity (AnalyzeEntity a)) => QueryEntity a

-- | 2 query entities in sequence
instance (QueryEntity a, QueryEntity b) => QueryEntity (a, b)

-- | 3 query entities in sequence
instance (QueryEntity a, QueryEntity b, QueryEntity c) => QueryEntity (a, b, c)

-- | 4 query entities in sequence
instance (QueryEntity a, QueryEntity b, QueryEntity c, QueryEntity d) => QueryEntity (a, b, c, d)

-- | 5 query entities in sequence
instance (QueryEntity a, QueryEntity b, QueryEntity c, QueryEntity d, QueryEntity e) => QueryEntity (a, b, c, d, e)

-- | 6 query entities in sequence
instance (QueryEntity a, QueryEntity b, QueryEntity c, QueryEntity d, QueryEntity e, QueryEntity f) => QueryEntity (a, b, c, d, e, f)

-- | 7 query entities in sequence
instance (QueryEntity a, QueryEntity b, QueryEntity c, QueryEntity d, QueryEntity e, QueryEntity f, QueryEntity g) => QueryEntity (a, b, c, d, e, f, g)

-- | 'QueryBuilder'
instance QueryEntity QueryBuilder where
	insertEntity = id

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
	insertTypedValue_ (Oid 1700) (showByteString x)

-- | Any integer
instance QueryEntity Integer where
	insertEntity = insertNumericValue

-- | Any integer
instance QueryEntity Int where
	insertEntity = insertNumericValue

-- | Any integer
instance QueryEntity Int8 where
	insertEntity = insertNumericValue

-- | Any integer
instance QueryEntity Int16 where
	insertEntity = insertNumericValue

-- | Any integer
instance QueryEntity Int32 where
	insertEntity = insertNumericValue

-- | Any integer
instance QueryEntity Int64 where
	insertEntity = insertNumericValue

-- | Any unsigned integer
instance QueryEntity Natural where
	insertEntity = insertNumericValue

-- | Any unsigned integer
instance QueryEntity Word where
	insertEntity = insertNumericValue

-- | Any unsigned integer
instance QueryEntity Word8 where
	insertEntity = insertNumericValue

-- | Any unsigned integer
instance QueryEntity Word16 where
	insertEntity = insertNumericValue

-- | Any unsigned integer
instance QueryEntity Word32 where
	insertEntity = insertNumericValue

-- | Any unsigned integer
instance QueryEntity Word64 where
	insertEntity = insertNumericValue

-- | Any floating-point number
instance QueryEntity Double where
	insertEntity = insertNumericValue

-- | Any floating-point number
instance QueryEntity Float where
	insertEntity = insertNumericValue

-- | Simplified version of 'insertTypedValue'
insertTypedValue_ :: Oid -> B.ByteString -> QueryBuilder
insertTypedValue_ typ val =
	insertTypedValue (TypedValue typ (Just (Value val)))

-- | Any numeric type
instance QueryEntity Scientific where
	insertEntity x =
		insertTypedValue_ (Oid 1700) (buildByteString (formatScientific Fixed Nothing x))

-- | @char@, @varchar@ or @text@ - UTF-8 encoded
instance QueryEntity String where
	insertEntity value =
		insertTypedValue_ (Oid 25) (buildByteString value)

-- | @char@, @varchar@ or @text@ - UTF-8 encoded
instance QueryEntity T.Text where
	insertEntity value =
		insertTypedValue_ (Oid 25) (T.encodeUtf8 value)

-- | @char@, @varchar@ or @text@ - UTF-8 encoded
instance QueryEntity TL.Text where
	insertEntity value =
		insertEntity (TL.toStrict value)

-- | @bytea@ - byte array encoded in hex format
instance QueryEntity B.ByteString where
	insertEntity value =
		insertTypedValue_ (Oid 17) dat
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
		insertTypedValue_ (Oid 114) (BL.toStrict (A.encode value))
