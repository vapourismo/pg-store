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
	GenericEntity
) where

import           GHC.Generics (Meta (..))
import           GHC.TypeLits hiding (Text)

import           Control.Applicative

import           Data.Int
import           Data.Word
import           Data.Bits
import           Data.Proxy
import           Data.Semigroup
import           Data.Scientific (Scientific, formatScientific, FPFormat (Fixed))
import           Numeric.Natural

import qualified Data.Aeson              as A

import           Data.Attoparsec.ByteString
import           Data.Attoparsec.ByteString.Char8 (signed, decimal, double, scientific, skipSpace)

import qualified Data.ByteString         as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy    as BL

import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import qualified Data.Text.Lazy          as TL

import           Database.PostgreSQL.Store.Types
import           Database.PostgreSQL.Store.Tuple
import           Database.PostgreSQL.Store.Generics
import           Database.PostgreSQL.Store.Utilities
import           Database.PostgreSQL.Store.RowParser
import           Database.PostgreSQL.Store.Query.Builder

-- | Generic record entity
class (KnownNat (GRecordWidth rec)) => GEntityRecord (rec :: KRecord) where
	type GRecordWidth rec :: Nat

	gEmbedRecord :: QueryGenerator (Record rec)

	gParseRecord :: RowParser (GRecordWidth rec) (Record rec)

instance (Entity typ) => GEntityRecord ('TSingle meta typ) where
	type GRecordWidth ('TSingle meta typ) = Width typ

	gEmbedRecord = With (\ (Single x) -> x) genEntity

	gParseRecord = Single <$> parseEntity

instance (GEntityRecord lhs,
          GEntityRecord rhs,
          KnownNat (GRecordWidth lhs + GRecordWidth rhs))
         => GEntityRecord ('TCombine lhs rhs) where

	type GRecordWidth ('TCombine lhs rhs) = GRecordWidth lhs + GRecordWidth rhs

	gEmbedRecord =
		mconcat [With (\ (Combine lhs _) -> lhs) gEmbedRecord,
		         Code ",",
		         With (\ (Combine _ rhs) -> rhs) gEmbedRecord]

	gParseRecord =
		Combine <$>  gParseRecord
		        <*>$ gParseRecord

-- | Generic enumeration value
class GEntityEnum (enum :: KFlatSum) where
	gEnumToPayload :: FlatSum enum -> B.ByteString

	gEnumFromPayload :: B.ByteString -> Maybe (FlatSum enum)

instance (KnownSymbol name) => GEntityEnum ('TValue ('MetaCons name f r)) where
	gEnumToPayload _ = buildByteString (symbolVal @name Proxy)

	gEnumFromPayload value
		| value == buildByteString (symbolVal @name Proxy) = Just Unit
		| otherwise                                        = Nothing

instance (GEntityEnum lhs, GEntityEnum rhs) => GEntityEnum ('TChoose lhs rhs) where
	gEnumToPayload (ChooseLeft lhs)  = gEnumToPayload lhs
	gEnumToPayload (ChooseRight rhs) = gEnumToPayload rhs

	gEnumFromPayload input =
		(ChooseLeft <$> gEnumFromPayload input) <|> (ChooseRight <$> gEnumFromPayload input)


-- | Generic entity
class (KnownNat (GEntityWidth dat)) => GEntity (dat :: KDataType) where
	type GEntityWidth dat :: Nat

	gEmbedEntity :: QueryGenerator (DataType dat)

	gParseEntity :: RowParser (GEntityWidth dat) (DataType dat)

instance (GEntityRecord rec) => GEntity ('TRecord d c rec) where
	type GEntityWidth ('TRecord d c rec) = GRecordWidth rec

	gEmbedEntity = With (\ (Record x) -> x) gEmbedRecord

	gParseEntity = Record <$> gParseRecord

instance (GEntityEnum enum) => GEntity ('TFlatSum d enum) where
	type GEntityWidth ('TFlatSum d enum) = 1

	gEmbedEntity = Gen (Oid 0) (\ (FlatSum x) -> Just (gEnumToPayload x))

	gParseEntity =
		retrieveContent >>=$ \ input ->
			case gEnumFromPayload input of
				Just x  -> finish (FlatSum x)
				Nothing -> cancel ColumnRejected

-- | This is required if you want to use the default implementations of 'genEntity'or 'parseEntity'
-- with polymorphic data types.
type GenericEntity a = (Generic a, GEntity (Rep a))

-- | Generic 'QueryGenerator' for an entity.
genGeneric :: (Generic a, GEntity (Rep a)) => QueryGenerator a
genGeneric = With fromGeneric gEmbedEntity

-- | Generic 'RowParser' for an entity.
parseGeneric :: (Generic a, GEntity (Rep a)) => RowParser (GEntityWidth (Rep a)) a
parseGeneric = toGeneric <$> gParseEntity

-- | An entity that is used as a parameter or result of a query.
class (KnownNat (Width a)) => Entity a where
	-- | Number of values the entity consists of
	type Width a :: Nat

	type Width a = GEntityWidth (Rep a)

	-- | Embed the entity into the query.
	genEntity :: QueryGenerator a

	default genEntity :: (Generic a, GEntity (Rep a)) => QueryGenerator a
	genEntity = genGeneric

	-- | Retrieve an instance of @a@ from the result set.
	parseEntity :: RowParser (Width a) a

	default parseEntity :: (Generic a, GEntity (Rep a))
	                    => RowParser (GEntityWidth (Rep a)) a
	parseEntity = parseGeneric

-- | Embed an entity into the query.
embedEntity :: (Entity e) => e -> QueryGenerator a
embedEntity e = withOther e genEntity

-- | Parameter entity at index 0
param0 :: (Entity r) => QueryGenerator (Tuple (r ': ts))
param0 = withParam0 genEntity

-- | Parameter entity at index 1
param1 :: (Entity r) => QueryGenerator (Tuple (t0 ': r ': ts))
param1 = withParam1 genEntity

-- | Parameter entity at index 2
param2 :: (Entity r) => QueryGenerator (Tuple (t0 ': t1 ': r ': ts))
param2 = withParam2 genEntity

-- | Parameter entity at index 3
param3 :: (Entity r) => QueryGenerator (Tuple (t0 ': t1 ': t2 ': r ': ts))
param3 = withParam3 genEntity

-- | Parameter entity at index 4
param4 :: (Entity r) => QueryGenerator (Tuple (t0 ': t1 ': t2 ': t3 ': r ': ts))
param4 = withParam4 genEntity

-- | Parameter entity at index 5
param5 :: (Entity r) => QueryGenerator (Tuple (t0 ': t1 ': t2 ': t3 ': t4 ': r ': ts))
param5 = withParam5 genEntity

-- | Parameter entity at index 6
param6 :: (Entity r) => QueryGenerator (Tuple (t0 ': t1 ': t2 ': t3 ': t4 ': t5 ': r ': ts))
param6 = withParam6 genEntity

-- | Parameter entity at index 7
param7 :: (Entity r) => QueryGenerator (Tuple (t0 ': t1 ': t2 ': t3 ': t4 ': t5 ': t6 ': r ': ts))
param7 = withParam7 genEntity

-- | Parameter entity at index 8
param8 :: (Entity r) => QueryGenerator (Tuple (t0 ': t1 ': t2 ': t3 ': t4 ': t5 ': t6 ': t7 ': r ': ts))
param8 = withParam8 genEntity

-- | Parameter entity at index 9
param9 :: (Entity r) => QueryGenerator (Tuple (t0 ': t1 ': t2 ': t3 ': t4 ': t5 ': t6 ': t7 ': t8 ': r ': ts))
param9 = withParam9 genEntity

-- | Chain of 2 entities
instance (GenericEntity (a, b)) => Entity (a, b)

-- | Chain of 3 entities
instance (GenericEntity (a, b, c)) => Entity (a, b, c)

-- | Chain of 4 entities
instance (GenericEntity (a, b, c, d)) => Entity (a, b, c, d)

-- | Chain of 5 entities
instance (GenericEntity (a, b, c, d, e)) => Entity (a, b, c, d, e)

-- | Chain of 6 entities
instance (GenericEntity (a, b, c, d, e, f)) => Entity (a, b, c, d, e, f)

-- | Chain of 7 entities
instance (GenericEntity (a, b, c, d, e, f, g)) => Entity (a, b, c, d, e, f, g)

-- | A value which may be @NULL@.
instance (Entity a) => Entity (Maybe a) where
	type Width (Maybe a) = Width a

	genEntity =
		walkTree genEntity
		where
			walkTree :: QueryGenerator b -> QueryGenerator (Maybe b)
			walkTree (Gen oid f)  = Gen oid (>>= f)
			walkTree (Code code)  = Code code
			walkTree (With f gen) = With (fmap f) (walkTree gen)
			walkTree (Merge l r)  = Merge (walkTree l) (walkTree r)

	parseEntity =
		nonNullCheck width >>=$ \ allNonNull ->
			if allNonNull then
				Just <$> parseEntity
			else
				skipColumns >>$ finish Nothing
		where
			width = fromIntegral (natVal @(Width a) Proxy)

-- | Construct a 'QueryGenerator' using a 'B.Builder'.
buildGen :: Oid -> (a -> B.Builder) -> QueryGenerator a
buildGen typ builder =
	Gen typ (Just . BL.toStrict . B.toLazyByteString . builder)

-- | Parse the contents of a column.
parseContent :: Parser a -> RowParser 1 a
parseContent p =
	processContent $ \ _ mbCnt -> do
		r <- mbCnt
		case endResult (parse p r) of
			Done _ r -> Just r
			_        -> Nothing
	where
		endResult (Partial f) = f B.empty
		endResult x           = x

-- | @boolean@
instance Entity Bool where
	type Width Bool = 1

	genEntity = Gen (Oid 16) (\ v -> Just (if v then "t" else "f"))

	parseEntity =
		(`elem` ["t", "1", "true", "TRUE", "y", "yes", "YES", "on", "ON"]) <$> retrieveContent

-- | Any integer
instance Entity Integer where
	type Width Integer = 1

	genEntity = buildGen (Oid 1700) B.integerDec

	parseEntity = parseContent (signed decimal)

-- | Any integer
instance Entity Int where
	type Width Int = 1

	genEntity = buildGen (Oid 20) B.intDec

	parseEntity = parseContent (signed decimal)

-- | Any integer
instance Entity Int8 where
	type Width Int8 = 1

	genEntity = buildGen (Oid 21) B.int8Dec

	parseEntity = parseContent (signed decimal)

-- | Any integer
instance Entity Int16 where
	type Width Int16 = 1

	genEntity = buildGen (Oid 21) B.int16Dec

	parseEntity = parseContent (signed decimal)

-- | Any integer
instance Entity Int32 where
	type Width Int32 = 1

	genEntity = buildGen (Oid 23) B.int32Dec

	parseEntity = parseContent (signed decimal)

-- | Any integer
instance Entity Int64 where
	type Width Int64 = 1

	genEntity = buildGen (Oid 20) B.int64Dec

	parseEntity = parseContent (signed decimal)

-- | Any unsigned integer
instance Entity Natural where
	type Width Natural = 1

	genEntity = With toInteger genEntity

	parseEntity = parseContent decimal

-- | Any unsigned integer
instance Entity Word where
	type Width Word = 1

	genEntity = buildGen (Oid 1700) B.wordDec

	parseEntity = parseContent decimal

-- | Any unsigned integer
instance Entity Word8 where
	type Width Word8 = 1

	genEntity = buildGen (Oid 21) B.word8Dec

	parseEntity = parseContent decimal

-- | Any unsigned integer
instance Entity Word16 where
	type Width Word16 = 1

	genEntity = buildGen (Oid 23) B.word16Dec

	parseEntity = parseContent decimal

-- | Any unsigned integer
instance Entity Word32 where
	type Width Word32 = 1

	genEntity = buildGen (Oid 20) B.word32Dec

	parseEntity = parseContent decimal

-- | Any unsigned integer
instance Entity Word64 where
	type Width Word64 = 1

	genEntity = buildGen (Oid 1700) B.word64Dec

	parseEntity = parseContent decimal

-- | Any floating-point number
instance Entity Double where
	type Width Double = 1

	genEntity = buildGen (Oid 1700) B.doubleDec

	parseEntity = parseContent double

-- | Any floating-point number
instance Entity Float where
	type Width Float = 1

	genEntity = buildGen (Oid 1700) B.floatDec

	parseEntity = realToFrac @Double @Float <$> parseEntity

-- | Any numeric type
instance Entity Scientific where
	type Width Scientific = 1

	genEntity = Gen (Oid 1700) (Just . buildByteString . formatScientific Fixed Nothing)

	parseEntity = parseContent scientific

-- | @char@, @varchar@ or @text@ - UTF-8 encoded; does not allow NULL characters
instance Entity String where
	type Width String = 1

	genEntity = Gen (Oid 25) (Just . buildByteString . filter (/= '\NUL'))

	parseEntity = T.unpack <$> parseEntity

-- | @char@, @varchar@ or @text@ - UTF-8 encoded; does not allow NULL characters
instance Entity T.Text where
	type Width T.Text = 1

	genEntity = Gen (Oid 25) (Just . T.encodeUtf8 . T.filter (/= '\NUL'))

	parseEntity =
		retrieveContent >>=$ \ input ->
			case T.decodeUtf8' input of
				Right x -> finish x
				_       -> cancel ColumnRejected

-- | @char@, @varchar@ or @text@ - UTF-8 encoded; does not allow NULL characters
instance Entity TL.Text where
	type Width TL.Text = 1

	genEntity = With TL.toStrict genEntity

	parseEntity = TL.fromStrict <$> parseEntity

-- | @bytea@ - byte array encoded in hex format
instance Entity B.ByteString where
	type Width B.ByteString = 1

	genEntity =
		buildGen (Oid 17) (\ value -> mconcat (B.string7 "\\x" : map showHex (B.unpack value)))
		where
			showHex n
				| n <= 0xF  = B.char7 '0' <> B.word8Hex n
				| otherwise = B.word8Hex n

	parseEntity =
		parseContent (hexFormat <|> escapedFormat)
		where
			isHexChar x =
				(x >= 48 && x <= 57)     -- 0 - 9
				|| (x >= 65 && x <= 70)  -- A - F
				|| (x >= 97 && x <= 102) -- a - f

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

			isOctChar x = x >= 48 && x <= 55 -- 0 - 7

			octCharToWord x
				| isOctChar x = x - 48 -- 0
				| otherwise   = 0

			escapedWord = do
				word8 92 -- \
				a <- satisfy isOctChar
				b <- satisfy isOctChar
				c <- satisfy isOctChar

				pure (shiftL (octCharToWord a) 6 .|. shiftL (octCharToWord b) 3 .|. c)

			escapedBackslash = do
				word8 92 -- \
				word8 92

			escapedFormat =
				B.pack <$> many (escapedBackslash <|> escapedWord <|> anyWord8)

-- | @bytea@ - byte array encoded in hex format
instance Entity BL.ByteString where
	type Width BL.ByteString = 1

	genEntity = With BL.toStrict genEntity

	parseEntity = BL.fromStrict <$> parseEntity

-- | @json@ or @jsonb@
instance Entity A.Value where
	type Width A.Value = 1

	genEntity = Gen (Oid 114) (Just . BL.toStrict . A.encode)

	parseEntity =
		retrieveContent >>=$ \ input ->
			case A.decodeStrict input of
				Just x -> finish x
				_      -> cancel ColumnRejected
