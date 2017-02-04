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

	withEntity,

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

import           Control.Applicative

import           Data.Int
import           Data.Bits
import           Data.Word
import           Data.Scientific hiding (scientific)
import           Numeric.Natural

import qualified Data.Aeson              as A

import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy    as BL

import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TL

import           Data.Attoparsec.ByteString
import           Data.Attoparsec.ByteString.Char8 (signed, decimal, skipSpace, double, scientific)

import           Database.PostgreSQL.Store.Value
import           Database.PostgreSQL.Store.Generics
import           Database.PostgreSQL.Store.RowParser
import           Database.PostgreSQL.Store.Parameters
import           Database.PostgreSQL.Store.Query.Builder

-- | Generic record entity
class GEntityRecord (rec :: KRecord) where
	gEmbedRecord :: QueryGenerator (Record rec)

	gParseRecord :: RowParser (Record rec)

instance (Entity typ) => GEntityRecord ('TSingle meta typ) where
	gEmbedRecord =
		With (\ (Single x) -> x) genEntity

	gParseRecord =
		Single <$> parseEntity

instance (GEntityRecord lhs, GEntityRecord rhs) => GEntityRecord ('TCombine lhs rhs) where
	gEmbedRecord =
		mconcat [With (\ (Combine lhs _) -> lhs) gEmbedRecord,
		         Code ",",
		         With (\ (Combine _ rhs) -> rhs) gEmbedRecord]

	gParseRecord =
		Combine <$> gParseRecord <*> gParseRecord

-- | Generic entity
class GEntity (dat :: KDataType) where
	gEmbedEntity :: QueryGenerator (DataType dat)

	gParseEntity :: RowParser (DataType dat)

instance (GEntityRecord rec) => GEntity ('TRecord d c rec) where
	gEmbedEntity =
		With (\ (Record x) -> x) gEmbedRecord

	gParseEntity =
		Record <$> gParseRecord

instance (GEnumValue enum) => GEntity ('TFlatSum d enum) where
	gEmbedEntity =
		Gen (\ (FlatSum x) -> gEnumToValue x)

	gParseEntity =
		FlatSum <$> parseContents gEnumFromValue

-- | Generator for a generic entity
genGeneric :: (GenericEntity a, GEntity (AnalyzeEntity a)) => QueryGenerator a
genGeneric =
	With fromGenericEntity gEmbedEntity

-- | Generic 'RowParser' for an entity.
parseGeneric :: (GenericEntity a, GEntity (AnalyzeEntity a)) => RowParser a
parseGeneric =
	toGenericEntity <$> gParseEntity

-- | An entity that is used as a parameter or result of a query.
class Entity a where
	-- | Embed the entity into the query.
	genEntity :: QueryGenerator a

	default genEntity :: (GenericEntity a, GEntity (AnalyzeEntity a)) => QueryGenerator a
	genEntity = genGeneric

	-- | Retrieve an instance of @a@ from the result set.
	parseEntity :: RowParser a

	default parseEntity :: (GenericEntity a, GEntity (AnalyzeEntity a)) => RowParser a
	parseEntity = parseGeneric

-- | Embed an entity into the query generator.
withEntity :: (Entity e) => e -> QueryGenerator a
withEntity e = withOther e genEntity

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

-- | 2 result entities in sequence
instance (Entity a, Entity b) => Entity (a, b)

-- | 3 result entities in sequence
instance (Entity a, Entity b, Entity c) => Entity (a, b, c)

-- | 4 result entities in sequence
instance (Entity a, Entity b, Entity c, Entity d) => Entity (a, b, c, d)

-- | 5 result entities in sequence
instance (Entity a, Entity b, Entity c, Entity d, Entity e) => Entity (a, b, c, d, e)

-- | 6 result entities in sequence
instance (Entity a, Entity b, Entity c, Entity d, Entity e, Entity f) => Entity (a, b, c, d, e, f)

-- | 7 result entities in sequence
instance (Entity a, Entity b, Entity c, Entity d, Entity e, Entity f, Entity g) => Entity (a, b, c, d, e, f, g)

-- | A value which may normally not be @NULL@.
instance (IsValue a, Entity a) => Entity (Maybe a) where
	genEntity = genValue

	parseEntity = do
		mbValue <- peekColumn
		case mbValue of
			Null -> pure Nothing
			_     -> Just <$> parseEntity

-- | @boolean@
instance Entity Bool where
	genEntity = genValue

	parseEntity =
		parseContents $ \ dat ->
			Just (elem dat ["t", "1", "true", "TRUE", "y", "yes", "YES", "on", "ON"])

-- | Parse a column using the given 'Parser'.
parseContentsWith :: Parser a -> RowParser a
parseContentsWith p =
	parseContents (maybeResult . endResult . parse p)
	where
		endResult (Partial f) = f B.empty
		endResult x           = x

-- | Any integer
instance Entity Integer where
	genEntity = genValue

	parseEntity = parseContentsWith (signed decimal)

-- | Any integer
instance Entity Int where
	genEntity = genValue

	parseEntity = parseContentsWith (signed decimal)

-- | Any integer
instance Entity Int8 where
	genEntity = genValue

	parseEntity = parseContentsWith (signed decimal)

-- | Any integer
instance Entity Int16 where
	genEntity = genValue

	parseEntity = parseContentsWith (signed decimal)

-- | Any integer
instance Entity Int32 where
	genEntity = genValue

	parseEntity = parseContentsWith (signed decimal)

-- | Any integer
instance Entity Int64 where
	genEntity = genValue

	parseEntity = parseContentsWith (signed decimal)

-- | Any unsigned integer
instance Entity Natural where
	genEntity = genValue

	parseEntity = parseContentsWith decimal

-- | Any unsigned integer
instance Entity Word where
	genEntity = genValue

	parseEntity = parseContentsWith decimal

-- | Any unsigned integer
instance Entity Word8 where
	genEntity = genValue

	parseEntity = parseContentsWith decimal

-- | Any unsigned integer
instance Entity Word16 where
	genEntity = genValue

	parseEntity = parseContentsWith decimal

-- | Any unsigned integer
instance Entity Word32 where
	genEntity = genValue

	parseEntity = parseContentsWith decimal

-- | Any unsigned integer
instance Entity Word64 where
	genEntity = genValue

	parseEntity = parseContentsWith decimal

-- | Any floating-point number
instance Entity Double where
	genEntity = genValue

	parseEntity = parseContentsWith double

-- | Any floating-point number
instance Entity Float where
	genEntity = genValue

	parseEntity = (realToFrac :: Double -> Float) <$> parseEntity

-- | Any numeric type
instance Entity Scientific where
	genEntity = genValue

	parseEntity = parseContentsWith scientific

-- | @char@, @varchar@ or @text@ - UTF-8 encoded; does not allow NULL characters
instance Entity String where
	genEntity = genValue

	parseEntity = T.unpack <$> parseEntity

-- | @char@, @varchar@ or @text@ - UTF-8 encoded; does not allow NULL characters
instance Entity T.Text where
	genEntity = genValue

	parseEntity =
		parseContents (either (const Nothing) Just . T.decodeUtf8')

-- | @char@, @varchar@ or @text@ - UTF-8 encoded; does not allow NULL characters
instance Entity TL.Text where
	genEntity = genValue

	parseEntity =
		parseContents (either (const Nothing) Just . TL.decodeUtf8' . BL.fromStrict)

-- | @bytea@ - byte array encoded in hex format
instance Entity B.ByteString where
	genEntity = genValue

	parseEntity =
		parseContentsWith (hexFormat <|> escapedFormat)
		where
			isHexChar x =
				(x >= 48 && x <= 57)     -- 0 - 9
				|| (x >= 65 && x <= 70)  -- A - Z
				|| (x >= 97 && x <= 102) -- a - z

			hexCharToWord x
				| x >= 48 && x <= 57  = x - 48
				| x >= 65 && x <= 70  = x - 55
				| x >= 97 && x <= 102 = x - 87
				| otherwise           = 0

			hexWord = do
				skipSpace
				a <- satisfy isHexChar
				b <- satisfy isHexChar

				pure (shiftL (hexCharToWord a) 4 .|. hexCharToWord b)

			hexFormat = do
				word8 92  -- \
				word8 120 -- x
				B.pack <$> many hexWord <* skipSpace

			isOctChar x = x >= 48 && x <= 55

			octCharToWord x
				| isOctChar x = x - 48
				| otherwise   = 0

			escapedWord = do
				word8 92
				a <- satisfy isOctChar
				b <- satisfy isOctChar
				c <- satisfy isOctChar

				pure (shiftL (octCharToWord a) 6 .|. shiftL (octCharToWord b) 3 .|. c)

			escapedBackslash = do
				word8 92
				word8 92

			escapedFormat =
				B.pack <$> many (escapedBackslash <|> escapedWord <|> anyWord8)

-- | @bytea@ - byte array encoded in hex format
instance Entity BL.ByteString where
	genEntity = genValue

	parseEntity = BL.fromStrict <$> parseEntity

-- | @json@ or @jsonb@
instance Entity A.Value where
	genEntity = genValue

	parseEntity = parseContents A.decodeStrict
