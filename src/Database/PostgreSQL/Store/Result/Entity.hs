{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, DeriveFunctor,
             DefaultSignatures #-}

-- |
-- Module:     Database.PostgreSQL.Store.Result.Entity
-- Copyright:  (c) Ole Krüger 2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.Result.Entity (
	-- * Result Entity
	ResultEntity (..)
) where

import           Control.Monad
import           Control.Applicative

import           Data.Int
import           Data.Word
import           Numeric.Natural
import           Data.Bits

import qualified Data.Aeson              as A

import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy    as BL

import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TL

import           Data.Attoparsec.ByteString
import           Data.Attoparsec.ByteString.Char8 (signed, decimal)

import           Database.PostgreSQL.Store.Types
import           Database.PostgreSQL.Store.Result.Parser
import           Database.PostgreSQL.Store.Utilities

-- | An entity whose underlying information spans zero or more columns
class ResultEntity a where
	parseEntity :: RowParser a

	default parseEntity :: (Read a) => RowParser a
	parseEntity = parseContents readByteString

-- | 2 result entities in sequence
instance (ResultEntity a, ResultEntity b) => ResultEntity (a, b) where
	parseEntity =
		liftA2 (,) parseEntity parseEntity

-- | 3 result entities in sequence
instance (ResultEntity a, ResultEntity b, ResultEntity c) => ResultEntity (a, b, c) where
	parseEntity =
		liftA3 (,,) parseEntity parseEntity parseEntity

-- | 4 result entities in sequence
instance (ResultEntity a, ResultEntity b, ResultEntity c, ResultEntity d)
         => ResultEntity (a, b, c, d) where
	parseEntity =
		liftM4 (,,,) parseEntity parseEntity parseEntity parseEntity

-- | 5 result entities in sequence
instance (ResultEntity a, ResultEntity b, ResultEntity c, ResultEntity d, ResultEntity e)
         => ResultEntity (a, b, c, d, e) where
	parseEntity =
		liftM5 (,,,,) parseEntity parseEntity parseEntity parseEntity parseEntity

-- | 6 result entities in sequence
instance (ResultEntity a, ResultEntity b, ResultEntity c, ResultEntity d, ResultEntity e,
          ResultEntity f)
         => ResultEntity (a, b, c, d, e, f) where
	parseEntity =
		(,,,,,) <$> parseEntity <*> parseEntity <*> parseEntity <*> parseEntity <*> parseEntity
		        <*> parseEntity

-- | 7 result entities in sequence
instance (ResultEntity a, ResultEntity b, ResultEntity c, ResultEntity d, ResultEntity e,
          ResultEntity f, ResultEntity g)
         => ResultEntity (a, b, c, d, e, f, g) where
	parseEntity =
		(,,,,,,) <$> parseEntity <*> parseEntity <*> parseEntity <*> parseEntity <*> parseEntity
		         <*> parseEntity <*> parseEntity

-- | 8 result entities in sequence
instance (ResultEntity a, ResultEntity b, ResultEntity c, ResultEntity d, ResultEntity e,
          ResultEntity f, ResultEntity g, ResultEntity h)
         => ResultEntity (a, b, c, d, e, f, g, h) where
	parseEntity =
		(,,,,,,,) <$> parseEntity <*> parseEntity <*> parseEntity <*> parseEntity <*> parseEntity
		          <*> parseEntity <*> parseEntity <*> parseEntity

-- | Untyped column value
instance ResultEntity Value where
	parseEntity = parseColumn (\ (TypedValue _ value) -> Just value)

-- | Typed column value
instance ResultEntity TypedValue where
	parseEntity = fetchColumn

instance (ResultEntity a) => ResultEntity (Maybe a) where
	parseEntity = do
		TypedValue _ value <- peekColumn
		case value of
			NoValue -> pure Nothing
			_       -> Just <$> parseEntity

-- | @boolean@ - everything that is not a valid boolean value is 'False'
instance ResultEntity Bool where
	parseEntity =
		parseContents $ \ dat ->
			Just (elem dat ["true", "TRUE", "t", "y", "yes", "YES", "on", "ON", "1"])

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

-- | @char@, @varchar@ or @text@
instance ResultEntity String where
	parseEntity = T.unpack <$> parseEntity

-- | @char@, @varchar@ or @text@
instance ResultEntity T.Text where
	parseEntity =
		parseContents (either (const Nothing) Just . T.decodeUtf8')
-- | @char@, @varchar@ or @text@
instance ResultEntity TL.Text where
	parseEntity =
		parseContents (either (const Nothing) Just . TL.decodeUtf8' . BL.fromStrict)

-- | @bytea@
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
				a <- satisfy isHexChar
				b <- satisfy isHexChar

				pure (shiftL (hexCharToWord a) 4 .|. hexCharToWord b)

			hexFormat = do
				word8 92  -- \
				word8 120 -- x
				B.pack <$> many hexWord

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

-- | @bytea@
instance ResultEntity BL.ByteString where
	parseEntity = BL.fromStrict <$> parseEntity

instance ResultEntity A.Value where
	parseEntity = parseContents A.decodeStrict
