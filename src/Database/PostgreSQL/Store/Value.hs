{-# LANGUAGE OverloadedStrings,
             DataKinds,
             TypeFamilies,
             TypeOperators,
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

	-- genericToValue,
	-- genericFromValue,

	-- -- * Helpers
	GEnumValue (..),
	-- GValue (..),
	-- CompositeValue
) where

import           GHC.Generics
import           GHC.TypeLits

import           Control.Applicative

import           Data.Proxy

import           Data.Bits
import           Data.Int
import           Data.Word
import           Data.Scientific (Scientific)
import           Numeric.Natural

import qualified Data.Aeson as A

import           Data.Attoparsec.ByteString
import           Data.Attoparsec.ByteString.Char8 (signed, decimal, double, scientific, skipSpace)

import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy    as BL

import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy     as TL

import           Database.PostgreSQL.Store.Types
import           Database.PostgreSQL.Store.Generics
import           Database.PostgreSQL.Store.Utilities

-- -- | Generic record value
-- class GRecordValue (rec :: KRecord) where
-- 	gRecordToPayload :: Record rec -> B.ByteString

-- 	gRecordParser :: Parser (Record rec)

-- instance (IsValue typ) => GRecordValue ('TSingle meta typ) where
-- 	gRecordToPayload (Single x) =
-- 		formatCompositeSegment (toValue x)

-- 	gRecordParser = do
-- 		val <- compositeValueParser
-- 		maybe empty (pure . Single) (fromValue val)

-- instance (GRecordValue lhs, GRecordValue rhs) => GRecordValue ('TCombine lhs rhs) where
-- 	gRecordToPayload (Combine lhs rhs) =
-- 		gRecordToPayload lhs <> "," <> gRecordToPayload rhs

-- 	gRecordParser =
-- 		Combine <$> gRecordParser
-- 		        <*  word8 44
-- 		        <*> gRecordParser

-- | Generic enumeration value
class GEnumValue (enum :: KFlatSum) where
	gEnumToPayload :: FlatSum enum -> B.ByteString

	gEnumFromPayload :: B.ByteString -> Maybe (FlatSum enum)

instance (KnownSymbol name) => GEnumValue ('TValue ('MetaCons name f r)) where
	gEnumToPayload _ =
		buildByteString (symbolVal @name Proxy)

	gEnumFromPayload value
		| value == buildByteString (symbolVal @name Proxy) = Just Unit
		| otherwise                                        = Nothing

instance (GEnumValue lhs, GEnumValue rhs) => GEnumValue ('TChoose lhs rhs) where
	gEnumToPayload (ChooseLeft lhs)  = gEnumToPayload lhs
	gEnumToPayload (ChooseRight rhs) = gEnumToPayload rhs

	gEnumFromPayload input =
		(ChooseLeft <$> gEnumFromPayload input) <|> (ChooseRight <$> gEnumFromPayload input)

-- -- | Generic value
-- class GValue (dat :: KDataType) where
-- 	gToValue :: DataType dat -> B.ByteString

-- 	gFromValue :: Value -> Maybe (DataType dat)

-- instance (GEnumValue enum) => GValue ('TFlatSum d enum) where
-- 	gToValue (FlatSum x) = gEnumToPayload x

-- 	gFromValue (Value _ input) = FlatSum <$> gEnumFromPayload input
-- 	gFromValue _               = Nothing

-- instance (GRecordValue rec) => GValue ('TRecord d c rec) where
-- 	gToValue (Record x) =
-- 		"(" <> gRecordToPayload x <> ")"

-- 	gFromValue =
-- 		parseValue (word8 40 *> fmap Record gRecordParser <* word8 41)

-- -- | To 'Value', generically.
-- genericToValue :: (GenericEntity a, GValue (AnalyzeEntity a)) => a -> B.By
-- genericToValue x =
-- 	gToValue (fromGenericEntity x)

-- -- | From 'Value', generically.
-- genericFromValue :: (GenericEntity a, GValue (AnalyzeEntity a)) => Value -> Maybe a
-- genericFromValue value =
-- 	toGenericEntity <$> gFromValue value

-- | Methods for converting from and to 'Value'
--
-- Implementation of 'toValue' and 'fromValue' may be omitted when @a@ satisfies 'GenericEntity'.
class IsValue a where
	-- default toValue :: (GenericEntity a, GValue (AnalyzeEntity a)) => a -> Value
	-- toValue = genericToValue

	-- | Parse 'Value'.
	fromValue :: Value -> Maybe a

	-- default fromValue :: (GenericEntity a, GValue (AnalyzeEntity a)) => Value -> Maybe a
	-- fromValue = genericFromValue

instance (IsValue a) => IsValue (Maybe a) where
	fromValue Null  = Just Nothing
	fromValue value = Just <$> fromValue value

-- instance (IsValue a, IsValue b) => IsValue (a, b)

-- instance (IsValue a, IsValue b, IsValue c) => IsValue (a, b, c)

-- instance (IsValue a, IsValue b, IsValue c, IsValue d) => IsValue (a, b, c, d)

-- instance (IsValue a, IsValue b, IsValue c, IsValue d, IsValue e) => IsValue (a, b, c, d, e)

-- instance (IsValue a, IsValue b, IsValue c, IsValue d, IsValue e, IsValue f) => IsValue (a, b, c, d, e, f)

-- instance (IsValue a, IsValue b, IsValue c, IsValue d, IsValue e, IsValue f, IsValue g) => IsValue (a, b, c, d, e, f, g)

-- -- | Parser for a single element of a composite.
-- compositeValueParser :: Parser Value
-- compositeValueParser =
-- 	(Value (Oid 0) . B.pack <$> (quotedValue <|> plainValue)) <|> pure Null
-- 	where
-- 		quotedValue = do
-- 			word8 34 -- "
-- 			many ((word8 34 >> word8 34) <|> notWord8 34)
-- 			<* word8 34

-- 		plainValue =
-- 			some $ satisfy $ \ n -> not $
-- 				n == 44 -- ,
-- 				|| n == 41 -- )
-- 				|| n == 40 -- (

-- -- | Format a composite segment so that it can be safely used.
-- formatCompositeSegment :: Value -> B.ByteString
-- formatCompositeSegment Null = B.empty
-- formatCompositeSegment (Value _ contents)
-- 	| B.null contents = "\"\""
-- 	| hasBadChar      = B.concat [B.singleton 34,
-- 	                              B.intercalate "\"\"" (B.split 34 contents),
-- 	                              B.singleton 34]
-- 	| otherwise       = contents
-- 	where
-- 		hasBadChar =
-- 			B.find (\ n -> n == 44 || n == 41 || n == 40 || n == 34) contents /= Nothing

-- -- | Helper class for @instance IsValue (Tuple ts)@
-- class CompositeValue (ts :: [Type]) where
-- 	compositeSegmentParser :: Parser (Tuple ts)

-- 	toCompositeSegment :: Tuple ts -> B.ByteString

-- instance {-# OVERLAPPABLE #-} (IsValue t, CompositeValue ts) => CompositeValue (t ': ts) where
-- 	compositeSegmentParser = do
-- 		val <- compositeValueParser
-- 		Cons <$> maybe empty pure (fromValue val)
-- 		     <*  word8 44
-- 		     <*> compositeSegmentParser

-- 	toCompositeSegment (Cons x xs) =
-- 		B.append (formatCompositeSegment (toValue x))
-- 		         (B.cons 44 (toCompositeSegment xs))

-- instance (IsValue t) => CompositeValue '[t] where
-- 	compositeSegmentParser = do
-- 		val <- compositeValueParser
-- 		(\ x -> Cons x Empty) <$> maybe empty pure (fromValue val)

-- 	toCompositeSegment (Cons x Empty) =
-- 		formatCompositeSegment (toValue x)

-- instance (CompositeValue xs) => IsValue (Tuple xs) where
-- 	toValue params =
-- 		Value (Oid 0) (B.concat ["(", toCompositeSegment params, ")"])

-- 	fromValue =
-- 		parseValue $ do
-- 			word8 40 -- (
-- 			compositeSegmentParser
-- 			<* word8 41 -- )

instance IsValue Value where
	fromValue = Just

instance IsValue Bool where
	fromValue (Value _ input) =
		Just (elem input ["t", "1", "true", "TRUE", "y", "yes", "YES", "on", "ON"])
	fromValue _ =
		Nothing

-- | Process the value's contents.
parseValue :: Parser a -> Value -> Maybe a
parseValue _ Null = Nothing
parseValue p (Value _ input) =
	maybeResult (endResult (parse p input))
	where
		endResult (Partial f) = f B.empty
		endResult x           = x

instance IsValue Integer where
	fromValue = parseValue (signed decimal)

instance IsValue Int where
	fromValue = parseValue (signed decimal)

instance IsValue Int8 where
	fromValue = parseValue (signed decimal)

instance IsValue Int16 where
	fromValue = parseValue (signed decimal)

instance IsValue Int32 where
	fromValue = parseValue (signed decimal)

instance IsValue Int64 where
	fromValue = parseValue (signed decimal)

instance IsValue Natural where
	fromValue = parseValue decimal

instance IsValue Word where
	fromValue = parseValue decimal

instance IsValue Word8 where
	fromValue = parseValue decimal

instance IsValue Word16 where
	fromValue = parseValue decimal

instance IsValue Word32 where
	fromValue = parseValue decimal

instance IsValue Word64 where
	fromValue = parseValue decimal

instance IsValue Double where
	fromValue = parseValue double

instance IsValue Float where
	fromValue value = realToFrac @Double @Float <$> fromValue value

instance IsValue Scientific where
	fromValue = parseValue scientific

instance IsValue String where
	fromValue value = T.unpack <$> fromValue value

instance IsValue T.Text where
	fromValue (Value _ input) = either (const Nothing) Just (T.decodeUtf8' input)
	fromValue _               = Nothing

instance IsValue TL.Text where
	fromValue value = TL.fromStrict <$> fromValue value

instance IsValue B.ByteString where
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
	fromValue value = BL.fromStrict <$> fromValue value

instance IsValue A.Value where
	fromValue (Value _ input) = A.decodeStrict input
	fromValue _               = Nothing
