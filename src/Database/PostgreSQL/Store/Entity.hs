{-# LANGUAGE OverloadedStrings,
             ConstraintKinds,
             DataKinds,
             DefaultSignatures,
             FlexibleContexts,
             FlexibleInstances,
             ScopedTypeVariables,
             TypeFamilies,
             TypeOperators,
             TypeApplications,
             TypeSynonymInstances,
             UndecidableInstances
#-}

-- |
-- Module:     Database.PostgreSQL.Store.Entity
-- Copyright:  (c) Ole Krüger 2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.Entity (
	-- * Result and query entity
	Entity (..),
	EntityC,

	embedEntity,

	param0,
	param1,
	param2,
	param3,
	param4,
	param5,
	param6,
	param7,
	param8,
	param9,

	genGeneric,
	parseGeneric,

	-- * Helpers
	GEntityRecord (..),
	GEntity (..),
	GenericPolyEntity
) where

import           GHC.TypeLits
import           Data.Kind

import           Data.Int
import           Data.Word
import           Data.Scientific hiding (scientific)
import           Numeric.Natural

import qualified Data.Aeson              as A

import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy    as BL

import qualified Data.Text               as T
import qualified Data.Text.Lazy          as TL

import           Database.PostgreSQL.Store.Value
import           Database.PostgreSQL.Store.Generics
import           Database.PostgreSQL.Store.RowParser
import           Database.PostgreSQL.Store.Parameters
import           Database.PostgreSQL.Store.Query.Builder

import           Database.PostgreSQL.LibPQ (Oid (..))

-- | Generic record entity
class GEntityRecord (rec :: KRecord) where
	type GRecordWidth rec :: Nat

	type GRecordConstraint rec :: Constraint

	gEmbedRecord :: QueryGenerator (Record rec)

	gParseRecord :: RowParser (GRecordWidth rec) (Record rec)

type GEntityRecordC a = (GEntityRecord a, KnownNat (GRecordWidth a))

instance (EntityC typ) => GEntityRecord ('TSingle meta typ) where
	type GRecordWidth ('TSingle meta typ) = Width typ

	type GRecordConstraint ('TSingle meta typ) = EntityC typ

	gEmbedRecord =
		With (\ (Single x) -> x) genEntity

	gParseRecord =
		withRowParser parseEntity (finish . Single)

instance (GEntityRecordC lhs, GEntityRecordC rhs) => GEntityRecord ('TCombine lhs rhs) where
	type GRecordWidth ('TCombine lhs rhs) = GRecordWidth lhs + GRecordWidth rhs

	type GRecordConstraint ('TCombine lhs rhs) = (GRecordConstraint lhs,
	                                              GRecordConstraint rhs,
	                                              KnownNat (GRecordWidth lhs + GRecordWidth rhs))

	gEmbedRecord =
		mconcat [With (\ (Combine lhs _) -> lhs) gEmbedRecord,
		         Code ",",
		         With (\ (Combine _ rhs) -> rhs) gEmbedRecord]

	gParseRecord =
		withRowParser gParseRecord $ \ lhs ->
			withRowParser gParseRecord $ \ rhs ->
				finish (Combine lhs rhs)

-- | Generic entity
class GEntity (dat :: KDataType) where
	type GEntityWidth dat :: Nat

	type GEntityConstraint dat :: Constraint

	gEmbedEntity :: QueryGenerator (DataType dat)

	gParseEntity :: RowParser (GEntityWidth dat) (DataType dat)

instance (GEntityRecordC rec) => GEntity ('TRecord d c rec) where
	type GEntityWidth ('TRecord d c rec) = GRecordWidth rec

	type GEntityConstraint ('TRecord d c rec) = GRecordConstraint rec

	gEmbedEntity =
		With (\ (Record x) -> x) gEmbedRecord

	gParseEntity =
		withRowParser gParseRecord (finish . Record)

instance (GEnumValue enum) => GEntity ('TFlatSum d enum) where
	type GEntityWidth ('TFlatSum d enum) = 1

	type GEntityConstraint ('TFlatSum d enum) = KnownNat 1

	gEmbedEntity =
		Gen (Oid 0) (\ (FlatSum x) -> Value (Oid 0) (gEnumToPayload x))

	gParseEntity =
		withEntityParser $ \ value ->
			case value of
				Value _ input ->
					case gEnumFromPayload input of
						Just x  -> finish (FlatSum x)
						Nothing -> cancel (ColumnRejected value)

				Null -> cancel (ColumnRejected value)

-- | 'GEntity' constraint and check that 'GEntityWidth' is a known natural number.
type GEntityC a = (GEntity a, KnownNat (GEntityWidth a))

-- | Required for instances of 'Entity' for types with type parameters. This constraints all
-- parameters of that type to be usable in a generic instance implementation.
--
-- This constraint mostly exists to shut the compiler up, because it can't know when type
-- variables @a@ and @b@ satisfy @(KnownNat a, KnownNat b)@ that @a + b@ satisfies
-- @KnownNat (a + b)@ (at least in practise).
--
-- Check the 'Entity' intances for tuple types to see examples.
type GenericPolyEntity a = (GEntityC (AnalyzeEntity a), GEntityConstraint (AnalyzeEntity a))

-- | Generic 'QueryGenerator' for an entity.
genGeneric :: (GenericEntity a, GEntity (AnalyzeEntity a)) => QueryGenerator a
genGeneric = With fromGenericEntity gEmbedEntity

-- | Generic 'RowParser' for an entity.
parseGeneric :: (GenericEntity a, GEntityC (AnalyzeEntity a))
             => RowParser (GEntityWidth (AnalyzeEntity a)) a
parseGeneric = withRowParser gParseEntity (finish . toGenericEntity)

-- | An entity that is used as a parameter or result of a query.
class Entity a where
	-- | Number of values the entity consists of
	type Width a :: Nat

	type Width a = GEntityWidth (AnalyzeEntity a)

	-- | Embed the entity into the query.
	genEntity :: QueryGenerator a

	default genEntity :: (GenericEntity a, GEntity (AnalyzeEntity a)) => QueryGenerator a
	genEntity = genGeneric

	-- | Retrieve an instance of @a@ from the result set.
	parseEntity :: RowParser (Width a) a

	default parseEntity :: (GenericEntity a, GEntityC (AnalyzeEntity a))
	                    => RowParser (GEntityWidth (AnalyzeEntity a)) a
	parseEntity = parseGeneric

-- | 'Entity' constraint plus a check that ensures 'Width' is a known natural number.
type EntityC a = (Entity a, KnownNat (Width a))

-- | Feed the given function the result of an entity's row parser.
withEntityParser :: (EntityC a) => (a -> RowParser w b) -> RowParser (Width a + w) b
withEntityParser = withRowParser parseEntity

-- | Embed an entity into the query.
embedEntity :: (Entity e) => e -> QueryGenerator a
embedEntity e = withOther e genEntity

-- | Parameter entity at index 0
param0 :: (Entity r) => QueryGenerator (Parameters (r ': ts))
param0 = withParam0 genEntity

-- | Parameter entity at index 1
param1 :: (Entity r) => QueryGenerator (Parameters (t0 ': r ': ts))
param1 = withParam1 genEntity

-- | Parameter entity at index 2
param2 :: (Entity r) => QueryGenerator (Parameters (t0 ': t1 ': r ': ts))
param2 = withParam2 genEntity

-- | Parameter entity at index 3
param3 :: (Entity r) => QueryGenerator (Parameters (t0 ': t1 ': t2 ': r ': ts))
param3 = withParam3 genEntity

-- | Parameter entity at index 4
param4 :: (Entity r) => QueryGenerator (Parameters (t0 ': t1 ': t2 ': t3 ': r ': ts))
param4 = withParam4 genEntity

-- | Parameter entity at index 5
param5 :: (Entity r) => QueryGenerator (Parameters (t0 ': t1 ': t2 ': t3 ': t4 ': r ': ts))
param5 = withParam5 genEntity

-- | Parameter entity at index 6
param6 :: (Entity r) => QueryGenerator (Parameters (t0 ': t1 ': t2 ': t3 ': t4 ': t5 ': r ': ts))
param6 = withParam6 genEntity

-- | Parameter entity at index 7
param7 :: (Entity r) => QueryGenerator (Parameters (t0 ': t1 ': t2 ': t3 ': t4 ': t5 ': t6 ': r ': ts))
param7 = withParam7 genEntity

-- | Parameter entity at index 8
param8 :: (Entity r) => QueryGenerator (Parameters (t0 ': t1 ': t2 ': t3 ': t4 ': t5 ': t6 ': t7 ': r ': ts))
param8 = withParam8 genEntity

-- | Parameter entity at index 9
param9 :: (Entity r) => QueryGenerator (Parameters (t0 ': t1 ': t2 ': t3 ': t4 ': t5 ': t6 ': t7 ': t8 ': r ': ts))
param9 = withParam9 genEntity

-- | Chain of 2 entities
instance (GenericPolyEntity (a, b)) => Entity (a, b)

-- | Chain of 3 entities
instance (GenericPolyEntity (a, b, c)) => Entity (a, b, c)

-- | Chain of 4 entities
instance (GenericPolyEntity (a, b, c, d)) => Entity (a, b, c, d)

-- | Chain of 5 entities
instance (GenericPolyEntity (a, b, c, d, e)) => Entity (a, b, c, d, e)

-- | Chain of 6 entities
instance (GenericPolyEntity (a, b, c, d, e, f)) => Entity (a, b, c, d, e, f)

-- | Chain of 7 entities
instance (GenericPolyEntity (a, b, c, d, e, f, g)) => Entity (a, b, c, d, e, f, g)

-- | A value which may normally not be @NULL@.
instance (IsValue a) => Entity (Maybe a) where
	type Width (Maybe a) = 1

	genEntity = genValue

	parseEntity = parseFromValue

-- | Anything
instance Entity Value where
	type Width Value = 1

	genEntity = genValue

	parseEntity = parseValue

-- | @boolean@
instance Entity Bool where
	type Width Bool = 1

	genEntity = genValue

	parseEntity = parseFromValue

-- |
parseFromValue :: (IsValue a) => RowParser 1 a
parseFromValue =
	withEntityParser $ \ value ->
		case fromValue value of
			Just x  -> finish x
			Nothing -> cancel (ColumnRejected value)

-- | Any integer
instance Entity Integer where
	type Width Integer = 1

	genEntity = genValue

	parseEntity = parseFromValue

-- | Any integer
instance Entity Int where
	type Width Int = 1

	genEntity = genValue

	parseEntity = parseFromValue

-- | Any integer
instance Entity Int8 where
	type Width Int8 = 1

	genEntity = genValue

	parseEntity = parseFromValue

-- | Any integer
instance Entity Int16 where
	type Width Int16 = 1

	genEntity = genValue

	parseEntity = parseFromValue

-- | Any integer
instance Entity Int32 where
	type Width Int32 = 1

	genEntity = genValue

	parseEntity = parseFromValue

-- | Any integer
instance Entity Int64 where
	type Width Int64 = 1

	genEntity = genValue

	parseEntity = parseFromValue

-- | Any unsigned integer
instance Entity Natural where
	type Width Natural = 1

	genEntity = genValue

	parseEntity = parseFromValue

-- | Any unsigned integer
instance Entity Word where
	type Width Word = 1

	genEntity = genValue

	parseEntity = parseFromValue

-- | Any unsigned integer
instance Entity Word8 where
	type Width Word8 = 1

	genEntity = genValue

	parseEntity = parseFromValue

-- | Any unsigned integer
instance Entity Word16 where
	type Width Word16 = 1

	genEntity = genValue

	parseEntity = parseFromValue

-- | Any unsigned integer
instance Entity Word32 where
	type Width Word32 = 1

	genEntity = genValue

	parseEntity = parseFromValue

-- | Any unsigned integer
instance Entity Word64 where
	type Width Word64 = 1

	genEntity = genValue

	parseEntity = parseFromValue

-- | Any floating-point number
instance Entity Double where
	type Width Double = 1

	genEntity = genValue

	parseEntity = parseFromValue

-- | Any floating-point number
instance Entity Float where
	type Width Float = 1

	genEntity = genValue

	parseEntity = parseFromValue

-- | Any numeric type
instance Entity Scientific where
	type Width Scientific = 1

	genEntity = genValue

	parseEntity = parseFromValue

-- | @char@, @varchar@ or @text@ - UTF-8 encoded; does not allow NULL characters
instance Entity String where
	type Width String = 1

	genEntity = genValue

	parseEntity = parseFromValue

-- | @char@, @varchar@ or @text@ - UTF-8 encoded; does not allow NULL characters
instance Entity T.Text where
	type Width T.Text = 1

	genEntity = genValue

	parseEntity = parseFromValue

-- | @char@, @varchar@ or @text@ - UTF-8 encoded; does not allow NULL characters
instance Entity TL.Text where
	type Width TL.Text = 1

	genEntity = genValue

	parseEntity = parseFromValue

-- | @bytea@ - byte array encoded in hex format
instance Entity B.ByteString where
	type Width B.ByteString = 1

	genEntity = genValue

	parseEntity = parseFromValue

-- | @bytea@ - byte array encoded in hex format
instance Entity BL.ByteString where
	type Width BL.ByteString = 1

	genEntity = genValue

	parseEntity = parseFromValue

-- | @json@ or @jsonb@
instance Entity A.Value where
	type Width A.Value = 1

	genEntity = genValue

	parseEntity = parseFromValue
