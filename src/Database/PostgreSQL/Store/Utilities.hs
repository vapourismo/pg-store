{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module:     Database.PostgreSQL.Store.Utilities
-- Copyright:  (c) Ole Krüger 2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.Utilities (
	showByteString,
	liftByteString
) where

import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax

import qualified Data.ByteString                    as B
import qualified Blaze.ByteString.Builder           as B
import qualified Blaze.ByteString.Builder.Char.Utf8 as B

-- | Show as 'ByteString'
showByteString :: (Show a) => a -> B.ByteString
showByteString =
	B.toByteString . B.fromString . show

-- | Lift "ByteString".
liftByteString :: B.ByteString -> Q Exp
liftByteString bs =
	[e| B.pack $(lift (B.unpack bs)) |]
