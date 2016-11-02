{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, DefaultSignatures,
             TypeOperators, ScopedTypeVariables, ConstraintKinds, DataKinds, TypeFamilies,
             UndecidableInstances #-}
-- |
-- Module:     Database.PostgreSQL.Store.Result.Entity
-- Copyright:  (c) Ole Krüger 2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.Result.Entity (
	-- * Result Entity
	ResultEntity (..),

	-- * Generic Parser
	GenericResultEntity,
	parseGeneric,

	-- * Helpers
	GResultSel (..),
	GResultEnum (..),
	GResultCons (..)
) where

import           GHC.Generics
import           GHC.TypeLits

import           Control.Applicative

import           Data.Int
import           Data.Word
import           Data.Bits
import           Numeric.Natural

import           Data.Proxy
import           Data.Bifunctor

import qualified Data.Aeson              as A

import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy    as BL

import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TL

import           Data.Attoparsec.ByteString
import           Data.Attoparsec.ByteString.Char8 (signed, decimal, skipSpace)

import           Database.PostgreSQL.Store.Types
import           Database.PostgreSQL.Store.Utilities
import           Database.PostgreSQL.Store.Result.Parser

-- | @sel@ represents the selectors of a constructor.
class GResultSel sel where
	parseSel :: RowParser (sel x)

-- | Single selector
instance (ResultEntity a) => GResultSel (S1 meta (Rec0 a)) where
	parseSel = M1 . K1 <$> parseEntity

-- | Multiple selectors
instance (GResultSel lhs, GResultSel rhs) => GResultSel (lhs :*: rhs) where
	parseSel = (:*:) <$> parseSel <*> parseSel

-- | @cons@ represents the constructors of a data type.
class GResultCons cons where
	parseCons :: RowParser (cons x)

-- | Single constructor
instance (GResultSel sel) => GResultCons (C1 meta sel) where
	parseCons = M1 <$> parseSel

-- | Multiple constructors; each constructor must qualify as an enum value and can't have any
--   fields - the constructor that will be chosen is determined using a single column, which
--   contains the name of the constructor.
instance (GResultEnum lhs, GResultEnum rhs) => GResultCons (lhs :+: rhs) where
	parseCons =
		parseContents (`lookup` enumValues)

-- | @enum@ represents the constructors without selectors.
class GResultEnum enum where
	enumValues :: [(B.ByteString, enum x)]

-- | Single constructor
instance (KnownSymbol name) => GResultEnum (C1 ('MetaCons name meta1 meta2) U1) where
	enumValues =
		[(buildByteString (symbolVal (Proxy :: Proxy name)), M1 U1)]

-- | Multiple constructors
instance (GResultEnum lhs, GResultEnum rhs) => GResultEnum (lhs :+: rhs) where
	enumValues =
		map (second L1) enumValues
		++ map (second R1) enumValues

-- | Constrain @a@ to be a data type that satisfies one of the following properties:
--
--   * single constructor with 1 or more fields
--   * multiple constructors with no fields
--
type GenericResultEntity meta cons a = (Generic a, Rep a ~ D1 meta cons, GResultCons cons)

-- | 'RowParser' for a generic data type.
parseGeneric :: (GenericResultEntity meta cons a) => RowParser a
parseGeneric = to . M1 <$> parseCons

-- | An entity whose underlying information spans zero or more columns
class ResultEntity a where
	-- | Build an instance of @a@.
	parseEntity :: RowParser a

	default parseEntity :: (GenericResultEntity meta cons a) => RowParser a
	parseEntity = parseGeneric

-- | Generic instance
instance {-# OVERLAPPABLE #-} (GenericResultEntity meta cons a) => ResultEntity a where
	parseEntity = parseGeneric

-- | 2 result entities in sequence
instance (ResultEntity a, ResultEntity b) => ResultEntity (a, b)

-- | 3 result entities in sequence
instance (ResultEntity a, ResultEntity b, ResultEntity c) => ResultEntity (a, b, c)

-- | 4 result entities in sequence
instance (ResultEntity a, ResultEntity b, ResultEntity c, ResultEntity d)
         => ResultEntity (a, b, c, d)

-- | 5 result entities in sequence
instance (ResultEntity a, ResultEntity b, ResultEntity c, ResultEntity d, ResultEntity e)
         => ResultEntity (a, b, c, d, e)

-- | 6 result entities in sequence
instance (ResultEntity a, ResultEntity b, ResultEntity c, ResultEntity d, ResultEntity e,
          ResultEntity f)
         => ResultEntity (a, b, c, d, e, f)

-- | 7 result entities in sequence
instance (ResultEntity a, ResultEntity b, ResultEntity c, ResultEntity d, ResultEntity e,
          ResultEntity f, ResultEntity g)
         => ResultEntity (a, b, c, d, e, f, g)

-- | Untyped column value
instance ResultEntity Value where
	parseEntity = parseColumn (\ (TypedValue _ mbValue) -> mbValue)

-- | Typed column value
instance ResultEntity TypedValue where
	parseEntity = fetchColumn

-- | A value which may normally not be @NULL@.
instance (ResultEntity a) => ResultEntity (Maybe a) where
	parseEntity = do
		TypedValue _ value <- peekColumn
		case value of
			Nothing -> pure Nothing
			_       -> Just <$> parseEntity

-- | @boolean@ - every value that is not a valid boolean is 'False'
instance ResultEntity Bool where
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

-- | Any numeric type
instance ResultEntity Integer where
	parseEntity = parseContentsWith (signed decimal)

-- | Any numeric type
instance ResultEntity Int where
	parseEntity = parseContentsWith (signed decimal)

-- | Any numeric type
instance ResultEntity Int8 where
	parseEntity = parseContentsWith (signed decimal)

-- | Any numeric type
instance ResultEntity Int16 where
	parseEntity = parseContentsWith (signed decimal)

-- | Any numeric type
instance ResultEntity Int32 where
	parseEntity = parseContentsWith (signed decimal)

-- | Any numeric type
instance ResultEntity Int64 where
	parseEntity = parseContentsWith (signed decimal)

-- | Any unsigned numeric type
instance ResultEntity Natural where
	parseEntity = parseContentsWith decimal

-- | Any unsigned numeric type
instance ResultEntity Word where
	parseEntity = parseContentsWith decimal

-- | Any unsigned numeric type
instance ResultEntity Word8 where
	parseEntity = parseContentsWith decimal

-- | Any unsigned numeric type
instance ResultEntity Word16 where
	parseEntity = parseContentsWith decimal

-- | Any unsigned numeric type
instance ResultEntity Word32 where
	parseEntity = parseContentsWith decimal

-- | Any unsigned numeric type
instance ResultEntity Word64 where
	parseEntity = parseContentsWith decimal

-- | @char@, @varchar@ or @text@ - UTF-8 encoded
instance ResultEntity String where
	parseEntity = T.unpack <$> parseEntity

-- | @char@, @varchar@ or @text@ - UTF-8 encoded
instance ResultEntity T.Text where
	parseEntity =
		parseContents (either (const Nothing) Just . T.decodeUtf8')

-- | @char@, @varchar@ or @text@ - UTF-8 encoded
instance ResultEntity TL.Text where
	parseEntity =
		parseContents (either (const Nothing) Just . TL.decodeUtf8' . BL.fromStrict)

-- | @bytea@ - byte array in hex or escape format
instance ResultEntity B.ByteString where
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

-- | @bytea@ - byte array encoded in hex or escape format
instance ResultEntity BL.ByteString where
	parseEntity = BL.fromStrict <$> parseEntity

-- | @json@ or @jsonb@
instance ResultEntity A.Value where
	parseEntity = parseContents A.decodeStrict
