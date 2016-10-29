{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

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

-- | An entity whose underlying information spans zero or more columns
class ResultEntity a where
	parseEntity :: RowParser a

instance (ResultEntity a, ResultEntity b) => ResultEntity (a, b) where
	parseEntity =
		liftA2 (,) parseEntity parseEntity

instance (ResultEntity a, ResultEntity b, ResultEntity c) => ResultEntity (a, b, c) where
	parseEntity =
		liftA3 (,,) parseEntity parseEntity parseEntity

instance (ResultEntity a, ResultEntity b, ResultEntity c, ResultEntity d)
         => ResultEntity (a, b, c, d) where
	parseEntity =
		liftM4 (,,,) parseEntity parseEntity parseEntity parseEntity

instance (ResultEntity a, ResultEntity b, ResultEntity c, ResultEntity d, ResultEntity e)
         => ResultEntity (a, b, c, d, e) where
	parseEntity =
		liftM5 (,,,,) parseEntity parseEntity parseEntity parseEntity parseEntity

instance (ResultEntity a, ResultEntity b, ResultEntity c, ResultEntity d, ResultEntity e,
          ResultEntity f)
         => ResultEntity (a, b, c, d, e, f) where
	parseEntity =
		(,,,,,) <$> parseEntity <*> parseEntity <*> parseEntity <*> parseEntity <*> parseEntity
		        <*> parseEntity

instance (ResultEntity a, ResultEntity b, ResultEntity c, ResultEntity d, ResultEntity e,
          ResultEntity f, ResultEntity g)
         => ResultEntity (a, b, c, d, e, f, g) where
	parseEntity =
		(,,,,,,) <$> parseEntity <*> parseEntity <*> parseEntity <*> parseEntity <*> parseEntity
		         <*> parseEntity <*> parseEntity

instance (ResultEntity a, ResultEntity b, ResultEntity c, ResultEntity d, ResultEntity e,
          ResultEntity f, ResultEntity g, ResultEntity h)
         => ResultEntity (a, b, c, d, e, f, g, h) where
	parseEntity =
		(,,,,,,,) <$> parseEntity <*> parseEntity <*> parseEntity <*> parseEntity <*> parseEntity
		          <*> parseEntity <*> parseEntity <*> parseEntity

instance ResultEntity Value where
	parseEntity = parseColumn (\ (TypedValue _ value) -> Just value)

instance ResultEntity TypedValue where
	parseEntity = fetchColumn

instance (ResultEntity a) => ResultEntity (Maybe a) where
	parseEntity = do
		TypedValue _ value <- peekColumn
		case value of
			NoValue -> pure Nothing
			_       -> Just <$> parseEntity

-- | Parse the contents of a column (only if present).
parseContents :: (B.ByteString -> Maybe a) -> RowParser a
parseContents proc =
	parseColumn $ \ (TypedValue _ value) ->
		case value of
			Value dat -> proc dat
			NoValue   -> Nothing

instance ResultEntity Bool where
	parseEntity =
		parseContents $ \ dat ->
			Just (elem dat ["true", "TRUE", "t", "y", "yes", "YES", "on", "ON", "1"])

-- | Parse a column using the given 'Parser'.
parseColumnWith :: Parser a -> RowParser a
parseColumnWith p = parseContents (maybeResult . parse p)

instance ResultEntity Integer where
	parseEntity = parseColumnWith (signed decimal)

instance ResultEntity Int where
	parseEntity = parseColumnWith (signed decimal)

instance ResultEntity Int8 where
	parseEntity = parseColumnWith (signed decimal)

instance ResultEntity Int16 where
	parseEntity = parseColumnWith (signed decimal)

instance ResultEntity Int32 where
	parseEntity = parseColumnWith (signed decimal)

instance ResultEntity Int64 where
	parseEntity = parseColumnWith (signed decimal)

instance ResultEntity Natural where
	parseEntity = parseColumnWith decimal

instance ResultEntity Word where
	parseEntity = parseColumnWith decimal

instance ResultEntity Word8 where
	parseEntity = parseColumnWith decimal

instance ResultEntity Word16 where
	parseEntity = parseColumnWith decimal

instance ResultEntity Word32 where
	parseEntity = parseColumnWith decimal

instance ResultEntity Word64 where
	parseEntity = parseColumnWith decimal

instance ResultEntity String where
	parseEntity =
		parseContents $ \ contents ->
			T.unpack <$> either (const Nothing) Just (T.decodeUtf8' contents)

instance ResultEntity T.Text where
	parseEntity =
		parseContents (either (const Nothing) Just . T.decodeUtf8')

instance ResultEntity TL.Text where
	parseEntity =
		parseContents (either (const Nothing) Just . TL.decodeUtf8' . BL.fromStrict)
