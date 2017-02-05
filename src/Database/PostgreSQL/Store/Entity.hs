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
	GEntity (..)
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

-- | Generic record entity
class GEntityRecord (rec :: KRecord) where
	type GRecordWidth rec :: Nat

	gEmbedRecord :: QueryGenerator (Record rec)

	gParseRecord :: RowParser (GRecordWidth rec) (Record rec)

type GEntityRecordC a = (GEntityRecord a, KnownNat (GRecordWidth a))

instance (EntityC typ) => GEntityRecord ('TSingle meta typ) where
	type GRecordWidth ('TSingle meta typ) = Width typ

	gEmbedRecord =
		With (\ (Single x) -> x) genEntity

	gParseRecord =
		withRowParser parseEntity (finish . Single)

instance (GEntityRecordC lhs, GEntityRecordC rhs) => GEntityRecord ('TCombine lhs rhs) where
	type GRecordWidth ('TCombine lhs rhs) = GRecordWidth lhs + GRecordWidth rhs

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

	gEmbedEntity :: QueryGenerator (DataType dat)

	gParseEntity :: RowParser (GEntityWidth dat) (DataType dat)

instance (GEntityRecordC rec) => GEntity ('TRecord d c rec) where
	type GEntityWidth ('TRecord d c rec) = GRecordWidth rec

	gEmbedEntity =
		With (\ (Record x) -> x) gEmbedRecord

	gParseEntity =
		withRowParser gParseRecord (finish . Record)

instance (GEnumValue enum) => GEntity ('TFlatSum d enum) where
	type GEntityWidth ('TFlatSum d enum) = 1

	gEmbedEntity =
		Gen (\ (FlatSum x) -> gEnumToValue x)

	gParseEntity =
		withEntityParser $ \ value ->
			case value of
				Value _ input ->
					case gEnumFromValue input of
						Just x  -> finish (FlatSum x)
						Nothing -> cancel (ColumnRejected value)

				Null -> cancel (ColumnRejected value)

type GEntityC a = (GEntity a, KnownNat (GEntityWidth a))

-- | Generator for a generic entity
genGeneric :: (GenericEntity a, GEntity (AnalyzeEntity a)) => QueryGenerator a
genGeneric =
	With fromGenericEntity gEmbedEntity

-- | Generic 'RowParser' for an entity.
parseGeneric :: (GenericEntity a, GEntityC (AnalyzeEntity a))
             => RowParser (GEntityWidth (AnalyzeEntity a)) a
parseGeneric =
	withRowParser gParseEntity (finish . toGenericEntity)

-- | An entity that is used as a parameter or result of a query.
class Entity a where
	type Width a :: Nat
	type Width a = GEntityWidth (AnalyzeEntity a)

	-- | Embed the entity into the query.
	genEntity :: QueryGenerator a

	default genEntity :: (GenericEntity a, GEntity (AnalyzeEntity a)) => QueryGenerator a
	genEntity = genGeneric

	-- | Retrieve an instance of @a@ from the result set.
	parseEntity :: RowParser (Width a) a

	default parseEntity :: (GenericEntity a, GEntityC (AnalyzeEntity a)) => RowParser (GEntityWidth (AnalyzeEntity a)) a
	parseEntity = parseGeneric

-- |
type EntityC a = (Entity a, KnownNat (Width a))

-- |
withEntityParser :: (EntityC a) => (a -> RowParser w b) -> RowParser (Width a + w) b
withEntityParser = withRowParser parseEntity

-- | Embed an entity into the query generator.
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

type family KnownWidth a :: Constraint where
	KnownWidth (a, b) = (KnownWidth a, KnownWidth b, KnownNat (Width a + Width b))
	KnownWidth (a, b, c) = KnownWidth (a, (b, c))
	KnownWidth (a, b, c, d) = KnownWidth ((a, b), c, d)
	KnownWidth (a, b, c, d, e) = KnownWidth ((a, b), c, (d, e))
	KnownWidth (a, b, c, d, e, f) = KnownWidth (a, (b, c), d, (e, f))
	KnownWidth (a, b, c, d, e, f, g) = KnownWidth (a, (b, c), (d, e), (f, g))
	KnownWidth a = KnownNat (Width a)

instance (EntityC a, EntityC b, KnownWidth (a, b)) => Entity (a, b)

instance (EntityC a, EntityC b, EntityC c,
          KnownWidth (a, b, c))
         => Entity (a, b, c)

instance (EntityC a, EntityC b, EntityC c, EntityC d,
          KnownWidth (a, b, c, d))
         => Entity (a, b, c, d)

instance (EntityC a, EntityC b, EntityC c, EntityC d, EntityC e,
          KnownWidth (a, b, c, d, e))
         => Entity (a, b, c, d, e)

instance (EntityC a, EntityC b, EntityC c, EntityC d, EntityC e, EntityC f,
          KnownWidth (a, b, c, d, e, f)) => Entity (a, b, c, d, e, f)

instance (EntityC a, EntityC b, EntityC c, EntityC d, EntityC e, EntityC f, EntityC g,
          KnownWidth (a, b, c, d, e, f, g)) => Entity (a, b, c, d, e, f, g)

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

	parseEntity =
		withEntityParser $ \ value ->
			case value of
				Value _ dat ->
					finish (elem dat ["t", "1", "true", "TRUE", "y", "yes", "YES", "on", "ON"])

				Null ->
					finish False

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
