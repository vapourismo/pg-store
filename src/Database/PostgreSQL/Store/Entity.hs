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

	genParam0,
	genParam1,
	genParam2,
	genParam3,
	genParam4,
	genParam5,
	genParam6,
	genParam7,
	genParam8,
	genParam9,

	genGeneric,
	parseGeneric,

	-- * Helpers
	GEntityRecord (..),
	GEntityEnum (..),
	GEntity (..)
) where

import           GHC.Generics
import           GHC.TypeLits

import           Control.Applicative

import           Data.Int
import           Data.Bits
import           Data.Word
import           Data.Scientific hiding (scientific)
import           Numeric
import           Numeric.Natural

import           Data.Bifunctor
import           Data.Proxy

import qualified Data.Aeson              as A
import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy    as BL
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TL

import           Data.Attoparsec.ByteString
import           Data.Attoparsec.ByteString.Char8 (signed, decimal, skipSpace, double, scientific)

import           Database.PostgreSQL.Store.Types
import           Database.PostgreSQL.Store.Utilities
import           Database.PostgreSQL.Store.Generics
import           Database.PostgreSQL.Store.RowParser
import           Database.PostgreSQL.Store.Parameters
import           Database.PostgreSQL.Store.Query.Builder2

import           Database.PostgreSQL.LibPQ (Oid (..), invalidOid)

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

-- | Generic enumeration entity
class GEntityEnum (enum :: KFlatSum) where
	gUnpackEnum :: FlatSum enum -> [Value]

	gEnumToValue :: FlatSum enum -> Value

	gEnumValues :: [(B.ByteString, FlatSum enum)]

instance (KnownSymbol name) => GEntityEnum ('TValue ('MetaCons name f r)) where
	gUnpackEnum _ =
		[Value invalidOid (buildByteString (symbolVal @name Proxy))]

	gEnumToValue _ =
		Value invalidOid (buildByteString (symbolVal @name Proxy))

	gEnumValues =
		[(buildByteString (symbolVal @name Proxy), Unit)]

instance (GEntityEnum lhs, GEntityEnum rhs) => GEntityEnum ('TChoose lhs rhs) where
	gUnpackEnum (ChooseLeft lhs)  = gUnpackEnum lhs
	gUnpackEnum (ChooseRight rhs) = gUnpackEnum rhs

	gEnumToValue (ChooseLeft lhs)  = gEnumToValue lhs
	gEnumToValue (ChooseRight rhs) = gEnumToValue rhs

	gEnumValues =
		map (second ChooseLeft) gEnumValues
		++ map (second ChooseRight) gEnumValues

-- | Generic entity
class GEntity (dat :: KDataType) where
	gEmbedEntity :: QueryGenerator (DataType dat)

	gParseEntity :: RowParser (DataType dat)

instance (GEntityRecord rec) => GEntity ('TRecord d c rec) where
	gEmbedEntity =
		With (\ (Record x) -> x) gEmbedRecord

	gParseEntity =
		Record <$> gParseRecord

instance (GEntityEnum enum) => GEntity ('TFlatSum d enum) where
	gEmbedEntity =
		Gen (\ (FlatSum x) -> gEnumToValue x)

	gParseEntity =
		FlatSum <$> parseContents (`lookup` gEnumValues)

-- |
genericToValue :: (GenericEntity a, AnalyzeEntity a ~ 'TFlatSum d e, GEntityEnum e) => a -> Value
genericToValue x =
	gEnumToValue enum
	where FlatSum enum = fromGenericEntity x

-- |
genGeneric :: (GenericEntity a, GEntity (AnalyzeEntity a)) => QueryGenerator a
genGeneric =
	With fromGenericEntity gEmbedEntity

-- | Generic 'RowParser' for an entity.
parseGeneric :: (GenericEntity a, GEntity (AnalyzeEntity a)) => RowParser a
parseGeneric =
	toGenericEntity <$> gParseEntity

-- |
class ToValue a where
	-- |
	toValue :: a -> Value

	default toValue :: (GenericEntity a, AnalyzeEntity a ~ 'TFlatSum d e, GEntityEnum e) => a -> Value
	toValue = genericToValue

-- |
genValue :: (ToValue a) => QueryGenerator a
genValue = Gen toValue

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

-- |
withEntity :: (Entity e) => e -> QueryGenerator a
withEntity e = withOther e genEntity

-- |
genParam0 :: (Entity r) => QueryGenerator (Parameters (r ': ts))
genParam0 = withParam0 genEntity

-- |
genParam1 :: (Entity r) => QueryGenerator (Parameters (t0 ': r ': ts))
genParam1 = withParam1 genEntity

-- |
genParam2 :: (Entity r) => QueryGenerator (Parameters (t0 ': t1 ': r ': ts))
genParam2 = withParam2 genEntity

-- |
genParam3 :: (Entity r) => QueryGenerator (Parameters (t0 ': t1 ': t2 ': r ': ts))
genParam3 = withParam3 genEntity

-- |
genParam4 :: (Entity r) => QueryGenerator (Parameters (t0 ': t1 ': t2 ': t3 ': r ': ts))
genParam4 = withParam4 genEntity

-- |
genParam5 :: (Entity r) => QueryGenerator (Parameters (t0 ': t1 ': t2 ': t3 ': t4 ': r ': ts))
genParam5 = withParam5 genEntity

-- |
genParam6 :: (Entity r) => QueryGenerator (Parameters (t0 ': t1 ': t2 ': t3 ': t4 ': t5 ': r ': ts))
genParam6 = withParam6 genEntity

-- |
genParam7 :: (Entity r) => QueryGenerator (Parameters (t0 ': t1 ': t2 ': t3 ': t4 ': t5 ': t6 ': r ': ts))
genParam7 = withParam7 genEntity

-- |
genParam8 :: (Entity r) => QueryGenerator (Parameters (t0 ': t1 ': t2 ': t3 ': t4 ': t5 ': t6 ': t7 ': r ': ts))
genParam8 = withParam8 genEntity

-- |
genParam9 :: (Entity r) => QueryGenerator (Parameters (t0 ': t1 ': t2 ': t3 ': t4 ': t5 ': t6 ': t7 ': t8 ': r ': ts))
genParam9 = withParam9 genEntity

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

instance (ToValue a) => ToValue (Maybe a) where
	toValue = maybe Null toValue

-- | A value which may normally not be @NULL@.
instance (ToValue a, Entity a) => Entity (Maybe a) where
	genEntity = genValue

	parseEntity = do
		mbValue <- peekColumn
		case mbValue of
			Null -> pure Nothing
			_     -> Just <$> parseEntity

-- |
instance ToValue Bool where
	toValue True = Value (Oid 16) "t"
	toValue _    = Value (Oid 16) "f"

-- | Comma-seperated list of entities
instance {-# OVERLAPPABLE #-} (Entity e) => Entity [e] where
	unpackEntity xs =
		concat (map unpackEntity xs)

	insertEntity xs =
		insertCommaSeperated (map insertEntity xs)

	parseEntity =
		many parseEntity

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

-- |
toNumericValue :: (Show a) => a -> Value
toNumericValue x =
	Value (Oid 1700) (showByteString x)

-- |
instance ToValue Integer where
	toValue = toNumericValue

-- | Any integer
instance Entity Integer where
	genEntity = genValue

	parseEntity = parseContentsWith (signed decimal)

-- |
instance ToValue Int where
	toValue = toNumericValue

-- | Any integer
instance Entity Int where
	genEntity = genValue

	parseEntity = parseContentsWith (signed decimal)

-- |
instance ToValue Int8 where
	toValue = toNumericValue

-- | Any integer
instance Entity Int8 where
	genEntity = genValue

	parseEntity = parseContentsWith (signed decimal)

-- |
instance ToValue Int16 where
	toValue = toNumericValue

-- | Any integer
instance Entity Int16 where
	genEntity = genValue

	parseEntity = parseContentsWith (signed decimal)

-- |
instance ToValue Int32 where
	toValue = toNumericValue

-- | Any integer
instance Entity Int32 where
	genEntity = genValue

	parseEntity = parseContentsWith (signed decimal)

-- |
instance ToValue Int64 where
	toValue = toNumericValue

-- | Any integer
instance Entity Int64 where
	genEntity = genValue

	parseEntity = parseContentsWith (signed decimal)

-- |
instance ToValue Natural where
	toValue = toNumericValue

-- | Any unsigned integer
instance Entity Natural where
	genEntity = genValue

	parseEntity = parseContentsWith decimal

-- |
instance ToValue Word where
	toValue = toNumericValue

-- | Any unsigned integer
instance Entity Word where
	genEntity = genValue

	parseEntity = parseContentsWith decimal

-- |
instance ToValue Word8 where
	toValue = toNumericValue

-- | Any unsigned integer
instance Entity Word8 where
	genEntity = genValue

	parseEntity = parseContentsWith decimal

-- |
instance ToValue Word16 where
	toValue = toNumericValue

-- | Any unsigned integer
instance Entity Word16 where
	genEntity = genValue

	parseEntity = parseContentsWith decimal

-- |
instance ToValue Word32 where
	toValue = toNumericValue

-- | Any unsigned integer
instance Entity Word32 where
	genEntity = genValue

	parseEntity = parseContentsWith decimal

-- |
instance ToValue Word64 where
	toValue = toNumericValue

-- | Any unsigned integer
instance Entity Word64 where
	genEntity = genValue

	parseEntity = parseContentsWith decimal

-- |
instance ToValue Double where
	toValue = toNumericValue

-- | Any floating-point number
instance Entity Double where
	genEntity = genValue

	parseEntity = parseContentsWith double

-- |
instance ToValue Float where
	toValue = toNumericValue

-- | Any floating-point number
instance Entity Float where
	genEntity = genValue

	parseEntity = (realToFrac :: Double -> Float) <$> parseEntity

-- |
instance ToValue Scientific where
	toValue x =
		Value (Oid 1700) (buildByteString (formatScientific Fixed Nothing x))

-- | Any numeric type
instance Entity Scientific where
	genEntity = genValue

	parseEntity = parseContentsWith scientific

-- |
instance ToValue String where
	toValue value =
		Value (Oid 25) (buildByteString (filter (/= '\NUL') value))

-- | @char@, @varchar@ or @text@ - UTF-8 encoded; does not allow NULL characters
instance Entity String where
	genEntity = genValue

	parseEntity = T.unpack <$> parseEntity

-- |
instance ToValue T.Text where
	toValue value =
		Value (Oid 25) (T.encodeUtf8 (T.concat (T.split (== '\NUL') value)))

-- | @char@, @varchar@ or @text@ - UTF-8 encoded; does not allow NULL characters
instance Entity T.Text where
	genEntity = genValue

	parseEntity =
		parseContents (either (const Nothing) Just . T.decodeUtf8')

-- |
instance ToValue TL.Text where
	toValue value =
		toValue (TL.toStrict value)

-- | @char@, @varchar@ or @text@ - UTF-8 encoded; does not allow NULL characters
instance Entity TL.Text where
	genEntity = genValue

	parseEntity =
		parseContents (either (const Nothing) Just . TL.decodeUtf8' . BL.fromStrict)

-- |
instance ToValue B.ByteString where
	toValue value =
		Value (Oid 17) dat
		where
			dat = B.append "\\x" (B.concatMap showHex' value)

			showHex' n =
				buildByteString $ case showHex n [] of
					(a : b : _) -> [a, b]
					(a : _)     -> ['0', a]
					[]          -> "00"

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

-- |
instance ToValue BL.ByteString where
	toValue value =
		toValue (BL.toStrict value)

-- | @bytea@ - byte array encoded in hex format
instance Entity BL.ByteString where
	genEntity = genValue

	parseEntity = BL.fromStrict <$> parseEntity

-- |
instance ToValue A.Value where
	toValue value =
		Value (Oid 114) (BL.toStrict (A.encode value))

-- | @json@ or @jsonb@
instance Entity A.Value where
	genEntity = genValue

	parseEntity = parseContents A.decodeStrict
