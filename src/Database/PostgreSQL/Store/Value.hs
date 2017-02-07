{-# LANGUAGE OverloadedStrings,
             DataKinds,
             TypeFamilies,
             TypeApplications,
             DefaultSignatures,
             KindSignatures,
             ScopedTypeVariables,
             FlexibleInstances,
             FlexibleContexts #-}

module Database.PostgreSQL.Store.Value (
	-- * General
	Value (..),

	IsValue (..),

	genericToValue,
	genericFromValue,

	-- * Helpers
	GEnumValue (..),
	GValue (..)
) where

import           GHC.Generics
import           GHC.TypeLits

import           Control.Applicative

import           Data.Proxy
import           Data.Tagged
import           Data.Monoid

import           Data.Bits
import           Data.Int
import           Data.Word
import           Data.Scientific (Scientific, formatScientific, FPFormat (Fixed))
import           Numeric.Natural

import qualified Data.Aeson as A

import           Data.Attoparsec.ByteString
import           Data.Attoparsec.ByteString.Char8 (signed, decimal, double, scientific, skipSpace)

import qualified Data.ByteString         as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy    as BL

import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy     as TL

import           Database.PostgreSQL.Store.Generics
import           Database.PostgreSQL.Store.Utilities

import           Database.PostgreSQL.LibPQ (Oid (..), invalidOid)

-- | Value sent with a request to the database
data Value
	= Value Oid B.ByteString
	| Null
	deriving (Show, Eq, Ord)

-- | Generic enumeration value
class GEnumValue (enum :: KFlatSum) where
	gEnumToValue :: FlatSum enum -> Value

	gEnumFromValue :: B.ByteString -> Maybe (FlatSum enum)

instance (KnownSymbol name) => GEnumValue ('TValue ('MetaCons name f r)) where
	gEnumToValue _ =
		Value invalidOid (buildByteString (symbolVal @name Proxy))

	gEnumFromValue value
		| value == buildByteString (symbolVal @name Proxy) = Just Unit
		| otherwise                                        = Nothing

instance (GEnumValue lhs, GEnumValue rhs) => GEnumValue ('TChoose lhs rhs) where
	gEnumToValue (ChooseLeft lhs)  = gEnumToValue lhs
	gEnumToValue (ChooseRight rhs) = gEnumToValue rhs

	gEnumFromValue input =
		(ChooseLeft <$> gEnumFromValue input) <|> (ChooseRight <$> gEnumFromValue input)

-- | Generic value
class GValue (dat :: KDataType) where
	gToValue :: DataType dat -> Value

	gFromValue :: Value -> Maybe (DataType dat)

instance (GEnumValue enum) => GValue ('TFlatSum d enum) where
	gToValue (FlatSum x) = gEnumToValue x

	gFromValue (Value _ input) = FlatSum <$> gEnumFromValue input
	gFromValue _               = Nothing

-- | To 'Value', generically.
genericToValue :: (GenericEntity a, GValue (AnalyzeEntity a)) => a -> Value
genericToValue x =
	gToValue (fromGenericEntity x)

-- | From 'Value', generically.
genericFromValue :: (GenericEntity a, GValue (AnalyzeEntity a)) => Value -> Maybe a
genericFromValue value =
	toGenericEntity <$> gFromValue value

-- | Encapsules methods for converting from and to 'Value'
class IsValue a where
	-- | Hint which 'Oid' this type will be associated with. This does restrict the input 'Value's
	-- to 'fromValue'. The implementaton can process 'Value's regardless of its 'Oid'.
	--
	-- Default 'Oid' is 0. This means the server will infer its type.
	oidOf :: Tagged a Oid
	oidOf = Tagged (Oid 0)

	-- | Transform to 'Value'.
	toValue :: a -> Value

	default toValue :: (GenericEntity a, GValue (AnalyzeEntity a)) => a -> Value
	toValue = genericToValue

	-- | Parse 'Value'.
	fromValue :: Value -> Maybe a

	default fromValue :: (GenericEntity a, GValue (AnalyzeEntity a)) => Value -> Maybe a
	fromValue = genericFromValue

instance (IsValue a) => IsValue (Maybe a) where
	oidOf = retag (oidOf @a)

	toValue (Just x) = toValue x
	toValue _        = Null

	fromValue Null  = Just Nothing
	fromValue value = Just <$> fromValue value

instance IsValue Value where
	toValue = id

	fromValue = Just

instance IsValue Bool where
	oidOf = Tagged (Oid 16)

	toValue True = Value (Oid 16) "t"
	toValue _    = Value (Oid 16) "f"

	fromValue (Value _ input) =
		Just (elem input ["t", "1", "true", "TRUE", "y", "yes", "YES", "on", "ON"])
	fromValue _ =
		Nothing

-- | Construct a 'Value' using a 'B.Builder'.
buildValue :: Oid -> (a -> B.Builder) -> a -> Value
buildValue typ builder x =
	Value typ (BL.toStrict (B.toLazyByteString (builder x)))

-- | Process the value's contents.
parseValue :: Parser a -> Value -> Maybe a
parseValue _ Null = Nothing
parseValue p (Value _ input) =
	maybeResult (endResult (parse p input))
	where
		endResult (Partial f) = f B.empty
		endResult x           = x

instance IsValue Integer where
	oidOf = Tagged (Oid 1700)

	toValue = buildValue (Oid 1700) B.integerDec

	fromValue = parseValue (signed decimal)

instance IsValue Int where
	oidOf = Tagged (Oid 20)

	toValue = buildValue (Oid 20) B.intDec

	fromValue = parseValue (signed decimal)

instance IsValue Int8 where
	oidOf = Tagged (Oid 21)

	toValue = buildValue (Oid 21) B.int8Dec

	fromValue = parseValue (signed decimal)

instance IsValue Int16 where
	oidOf = Tagged (Oid 21)

	toValue = buildValue (Oid 21) B.int16Dec

	fromValue = parseValue (signed decimal)

instance IsValue Int32 where
	oidOf = Tagged (Oid 23)

	toValue = buildValue (Oid 23) B.int32Dec

	fromValue = parseValue (signed decimal)

instance IsValue Int64 where
	oidOf = Tagged (Oid 20)

	toValue = buildValue (Oid 20) B.int64Dec

	fromValue = parseValue (signed decimal)

instance IsValue Natural where
	oidOf = retag (oidOf @Integer)

	toValue = toValue . toInteger

	fromValue = parseValue decimal

instance IsValue Word where
	oidOf = Tagged (Oid 1700)

	toValue = buildValue (Oid 1700) B.wordDec

	fromValue = parseValue decimal

instance IsValue Word8 where
	oidOf = Tagged (Oid 21)

	toValue = buildValue (Oid 21) B.word8Dec

	fromValue = parseValue decimal

instance IsValue Word16 where
	oidOf = Tagged (Oid 23)

	toValue = buildValue (Oid 23) B.word16Dec

	fromValue = parseValue decimal

instance IsValue Word32 where
	oidOf = Tagged (Oid 20)

	toValue = buildValue (Oid 20) B.word32Dec

	fromValue = parseValue decimal

instance IsValue Word64 where
	oidOf = Tagged (Oid 1700)

	toValue = buildValue (Oid 1700) B.word64Dec

	fromValue = parseValue decimal

instance IsValue Double where
	oidOf = Tagged (Oid 1700)

	toValue = buildValue (Oid 1700) B.doubleDec

	fromValue = parseValue double

instance IsValue Float where
	oidOf = Tagged (Oid 1700)

	toValue = buildValue (Oid 1700) B.floatDec

	fromValue value = realToFrac @Double @Float <$> fromValue value

instance IsValue Scientific where
	oidOf = Tagged (Oid 1700)

	toValue x = Value (Oid 1700) (buildByteString (formatScientific Fixed Nothing x))

	fromValue = parseValue scientific

instance IsValue String where
	oidOf = Tagged (Oid 25)

	toValue x = Value (Oid 25) (buildByteString (filter (/= '\NUL') x))

	fromValue value = T.unpack <$> fromValue value

instance IsValue T.Text where
	oidOf = Tagged (Oid 25)

	toValue x = Value (Oid 25) (T.encodeUtf8 (T.filter (/= '\NUL') x))

	fromValue (Value _ input) = either (const Nothing) Just (T.decodeUtf8' input)
	fromValue _               = Nothing

instance IsValue TL.Text where
	oidOf = Tagged (Oid 25)

	toValue x = toValue (TL.toStrict x)

	fromValue value = TL.fromStrict <$> fromValue value

instance IsValue B.ByteString where
	oidOf = Tagged (Oid 17)

	toValue =
		buildValue (Oid 17) (\ value -> mconcat (B.string7 "\\x" : map showHex (B.unpack value)))
		where
			showHex n
				| n <= 0xF  = B.char7 '0' <> B.word8Hex n
				| otherwise = B.word8Hex n

	fromValue =
		parseValue (hexFormat <|> escapedFormat)
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

instance IsValue BL.ByteString where
	oidOf = Tagged (Oid 17)

	toValue x = toValue (BL.toStrict x)

	fromValue value = BL.fromStrict <$> fromValue value

instance IsValue A.Value where
	oidOf = Tagged (Oid 114)

	toValue value = Value (Oid 114) (BL.toStrict (A.encode value))

	fromValue (Value _ input) = A.decodeStrict input
	fromValue _               = Nothing
