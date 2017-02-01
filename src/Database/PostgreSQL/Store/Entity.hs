{-# LANGUAGE OverloadedStrings,
             ConstraintKinds,
             DataKinds,
             DefaultSignatures,
             FlexibleContexts,
             FlexibleInstances,
             ScopedTypeVariables,
             TypeFamilies,
             TypeOperators,
             TypeSynonymInstances,
             UndecidableInstances,
             TypeApplications
#-}

-- |
-- Module:     Database.PostgreSQL.Store.Entity
-- Copyright:  (c) Ole Krüger 2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.Entity (
	-- * Result and query entity
	Entity (..),

	unpackGeneric,
	insertGeneric,
	parseGeneric,

	-- * Helpers
	GEntityRecord (..),
	GEntityEnum (..),
	GEntity (..)
) where

import           GHC.Generics
import           GHC.TypeLits

import           Control.Applicative
import           Control.Monad

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
import           Database.PostgreSQL.Store.Query.Builder
import           Database.PostgreSQL.Store.Query.Builder2
import           Database.PostgreSQL.Store.RowParser

import           Database.PostgreSQL.LibPQ (Oid (..), invalidOid)

-- | Generic record entity
class GEntityRecord (rec :: KRecord) where
	gUnpackRecord :: Record rec -> [TypedValue]

	gEmbedRecord :: QueryGenerator (Record rec)

	gParseRecord :: RowParser (Record rec)

instance (Entity typ) => GEntityRecord ('TSingle meta typ) where
	gUnpackRecord (Single x) =
		unpackEntity x

	gEmbedRecord =
		With (\ (Single x) -> x) embedEntity

	gParseRecord =
		Single <$> parseEntity

instance (GEntityRecord lhs, GEntityRecord rhs) => GEntityRecord ('TCombine lhs rhs) where
	gUnpackRecord (Combine lhs rhs) = do
		gUnpackRecord lhs
		++ gUnpackRecord rhs

	gEmbedRecord =
		mconcat [With (\ (Combine lhs _) -> lhs) gEmbedRecord,
		         Code ",",
		         With (\ (Combine _ rhs) -> rhs) gEmbedRecord]

	gParseRecord =
		Combine <$> gParseRecord <*> gParseRecord

-- | Generic enumeration entity
class GEntityEnum (enum :: KFlatSum) where
	gUnpackEnum :: FlatSum enum -> [TypedValue]

	gInsertEnum :: FlatSum enum -> QueryBuilder

	gEnumToValue :: FlatSum enum -> Value2

	gEnumValues :: [(B.ByteString, FlatSum enum)]

instance (KnownSymbol name) => GEntityEnum ('TValue ('MetaCons name f r)) where
	gUnpackEnum _ =
		[anyTypeValue (Value (buildByteString (symbolVal @name Proxy)))]

	gInsertEnum _ =
		insertQuote (buildByteString (symbolVal @name Proxy))

	gEnumToValue _ =
		Value2 invalidOid (buildByteString (symbolVal @name Proxy))

	gEnumValues =
		[(buildByteString (symbolVal @name Proxy), Unit)]

instance (GEntityEnum lhs, GEntityEnum rhs) => GEntityEnum ('TChoose lhs rhs) where
	gUnpackEnum (ChooseLeft lhs)  = gUnpackEnum lhs
	gUnpackEnum (ChooseRight rhs) = gUnpackEnum rhs

	gInsertEnum (ChooseLeft lhs)  = gInsertEnum lhs
	gInsertEnum (ChooseRight rhs) = gInsertEnum rhs

	gEnumToValue (ChooseLeft lhs)  = gEnumToValue lhs
	gEnumToValue (ChooseRight rhs) = gEnumToValue rhs

	gEnumValues =
		map (second ChooseLeft) gEnumValues
		++ map (second ChooseRight) gEnumValues

-- | Generic entity
class GEntity (dat :: KDataType) where
	gUnpackEntity :: DataType dat -> [TypedValue]

	gInsertEntity :: DataType dat -> QueryBuilder

	gEmbedEntity :: QueryGenerator (DataType dat)

	gParseEntity :: RowParser (DataType dat)

instance (GEntityRecord rec) => GEntity ('TRecord d c rec) where
	gUnpackEntity (Record x) =
		gUnpackRecord x

	gInsertEntity x =
		insertCommaSeperated (map insertTypedValue (gUnpackEntity x))

	gEmbedEntity =
		With (\ (Record x) -> x) gEmbedRecord

	gParseEntity =
		Record <$> gParseRecord

instance (GEntityEnum enum) => GEntity ('TFlatSum d enum) where
	gUnpackEntity (FlatSum x) =
		gUnpackEnum x

	gInsertEntity (FlatSum x) =
		gInsertEnum x

	gEmbedEntity =
		Gen (\ (FlatSum x) -> gEnumToValue x)

	gParseEntity =
		FlatSum <$> parseContents (`lookup` gEnumValues)

-- | Unpack a generic entity.
unpackGeneric :: (GenericEntity a, GEntity (AnalyzeEntity a)) => a -> [TypedValue]
unpackGeneric x =
	gUnpackEntity (fromGenericEntity x)

-- | Insert a generic entity into the query.
insertGeneric :: (GenericEntity a, GEntity (AnalyzeEntity a)) => a -> QueryBuilder
insertGeneric x =
	gInsertEntity (fromGenericEntity x)

-- |
embedGeneric :: (GenericEntity a, GEntity (AnalyzeEntity a)) => QueryGenerator a
embedGeneric =
	With fromGenericEntity gEmbedEntity

-- | Generic 'RowParser' for an entity.
parseGeneric :: (GenericEntity a, GEntity (AnalyzeEntity a)) => RowParser a
parseGeneric =
	toGenericEntity <$> gParseEntity

-- | An entity that is used as a parameter or result of a query.
class Entity a where
	-- |
	unpackEntity :: a -> [TypedValue]

	default unpackEntity :: (GenericEntity a, GEntity (AnalyzeEntity a)) => a -> [TypedValue]
	unpackEntity = unpackGeneric

	-- | Insert an instance of @a@ into the query.
	insertEntity :: a -> QueryBuilder
	insertEntity x =
		insertCommaSeperated (map insertTypedValue (unpackEntity x))

	-- | Embed the entity into the query.
	embedEntity :: QueryGenerator a
	embedEntity = error "Do not use yet"

	-- default embedEntity :: (GenericEntity a, GEntity (AnalyzeEntity a)) => QueryGenerator a
	-- embedEntity = embedGeneric

	-- | Retrieve an instance of @a@ from the result set.
	parseEntity :: RowParser a

	default parseEntity :: (GenericEntity a, GEntity (AnalyzeEntity a)) => RowParser a
	parseEntity = parseGeneric

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

-- | 'QueryBuilder'
instance Entity QueryBuilder where
	unpackEntity = buildQuery

	insertEntity = id

	parseEntity = do
		colsLeft <- columnsLeft
		insertCommaSeperated <$> replicateM (fromEnum colsLeft) (insertTypedValue <$> fetchColumn)

-- | Untyped column value
instance Entity Value where
	unpackEntity x = [anyTypeValue x]

	insertEntity = insertValue

	parseEntity = parseColumn (\ (TypedValue _ mbValue) -> mbValue)

-- | Typed column value
instance Entity TypedValue where
	unpackEntity x = [x]

	insertEntity = insertTypedValue

	parseEntity = fetchColumn

-- | A value which may normally not be @NULL@
instance (Entity a) => Entity (Maybe a) where
	unpackEntity Nothing  = [nullValue]
	unpackEntity (Just x) = unpackEntity x

	insertEntity Nothing  = insertTypedValue nullValue
	insertEntity (Just x) = insertEntity x

	parseEntity = do
		TypedValue _ value <- peekColumn
		case value of
			Nothing -> pure Nothing
			_       -> Just <$> parseEntity

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
	unpackEntity True = mkTypedValue (Oid 16) "t"
	unpackEntity _    = mkTypedValue (Oid 16) "f"

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

-- | Simplified version of 'insertTypedValue'
mkTypedValue :: Oid -> B.ByteString -> [TypedValue]
mkTypedValue typ val =
	[TypedValue typ (Just (Value val))]

-- | Insert a numeric value.
unpackNumeric :: (Show a) => a -> [TypedValue]
unpackNumeric x =
	mkTypedValue (Oid 1700) (showByteString x)

-- | Any integer
instance Entity Integer where
	unpackEntity = unpackNumeric

	parseEntity = parseContentsWith (signed decimal)

-- | Any integer
instance Entity Int where
	unpackEntity = unpackNumeric

	parseEntity = parseContentsWith (signed decimal)

-- | Any integer
instance Entity Int8 where
	unpackEntity = unpackNumeric

	parseEntity = parseContentsWith (signed decimal)

-- | Any integer
instance Entity Int16 where
	unpackEntity = unpackNumeric

	parseEntity = parseContentsWith (signed decimal)

-- | Any integer
instance Entity Int32 where
	unpackEntity = unpackNumeric

	parseEntity = parseContentsWith (signed decimal)

-- | Any integer
instance Entity Int64 where
	unpackEntity = unpackNumeric

	parseEntity = parseContentsWith (signed decimal)

-- | Any unsigned integer
instance Entity Natural where
	unpackEntity = unpackNumeric

	parseEntity = parseContentsWith decimal

-- | Any unsigned integer
instance Entity Word where
	unpackEntity = unpackNumeric

	parseEntity = parseContentsWith decimal

-- | Any unsigned integer
instance Entity Word8 where
	unpackEntity = unpackNumeric

	parseEntity = parseContentsWith decimal

-- | Any unsigned integer
instance Entity Word16 where
	unpackEntity = unpackNumeric

	parseEntity = parseContentsWith decimal

-- | Any unsigned integer
instance Entity Word32 where
	unpackEntity = unpackNumeric

	parseEntity = parseContentsWith decimal

-- | Any unsigned integer
instance Entity Word64 where
	unpackEntity = unpackNumeric

	parseEntity = parseContentsWith decimal

-- | Any floating-point number
instance Entity Double where
	unpackEntity = unpackNumeric

	parseEntity = parseContentsWith double

-- | Any floating-point number
instance Entity Float where
	unpackEntity = unpackNumeric

	parseEntity = (realToFrac :: Double -> Float) <$> parseEntity

-- | Any numeric type
instance Entity Scientific where
	unpackEntity x =
		mkTypedValue (Oid 1700) (buildByteString (formatScientific Fixed Nothing x))

	parseEntity = parseContentsWith scientific

-- | @char@, @varchar@ or @text@ - UTF-8 encoded; does not allow NULL characters
instance Entity String where
	unpackEntity value =
		mkTypedValue (Oid 25) (buildByteString (filter (/= '\NUL') value))

	parseEntity = T.unpack <$> parseEntity

-- | @char@, @varchar@ or @text@ - UTF-8 encoded; does not allow NULL characters
instance Entity T.Text where
	unpackEntity value =
		mkTypedValue (Oid 25) (T.encodeUtf8 (T.concat (T.split (== '\NUL') value)))

	parseEntity =
		parseContents (either (const Nothing) Just . T.decodeUtf8')

-- | @char@, @varchar@ or @text@ - UTF-8 encoded; does not allow NULL characters
instance Entity TL.Text where
	unpackEntity value =
		unpackEntity (TL.toStrict value)

	parseEntity =
		parseContents (either (const Nothing) Just . TL.decodeUtf8' . BL.fromStrict)

-- | @bytea@ - byte array encoded in hex format
instance Entity B.ByteString where
	unpackEntity value =
		mkTypedValue (Oid 17) dat
		where
			dat = B.append "\\x" (B.concatMap showHex' value)

			showHex' n =
				buildByteString $ case showHex n [] of
					(a : b : _) -> [a, b]
					(a : _)     -> ['0', a]
					[]          -> "00"

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
	unpackEntity value =
		unpackEntity (BL.toStrict value)

	parseEntity = BL.fromStrict <$> parseEntity

-- | @json@ or @jsonb@
instance Entity A.Value where
	unpackEntity value =
		mkTypedValue (Oid 114) (BL.toStrict (A.encode value))

	parseEntity = parseContents A.decodeStrict
