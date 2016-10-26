{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module:     Database.PostgreSQL.Store.Utilities
-- Copyright:  (c) Ole Krüger 2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.Utilities (
	showByteString,
	readByteString,
	liftByteString
) where

import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax

import qualified Data.ByteString                    as B
import qualified Blaze.ByteString.Builder           as B
import qualified Blaze.ByteString.Builder.Char.Utf8 as B
import qualified Data.Text                          as T
import qualified Data.Text.Encoding                 as T

import           Text.Read (readMaybe)

-- | Show as "ByteString"
showByteString :: (Show a) => a -> B.ByteString
showByteString =
	B.toByteString . B.fromString . show

-- | Read as "ByteString"
readByteString :: (Read a) => B.ByteString -> Maybe a
readByteString =
	readMaybe . T.unpack . T.decodeUtf8

-- | Lift "ByteString".
liftByteString :: B.ByteString -> Q Exp
liftByteString bs =
	[e| B.pack $(lift (B.unpack bs)) |]
