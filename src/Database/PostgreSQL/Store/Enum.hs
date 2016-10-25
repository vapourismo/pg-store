{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

-- |
-- Module:     Database.PostgreSQL.Store.Enum
-- Copyright:  (c) Ole Krüger 2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.Enum (
	-- * Helpers
	makeEnum
) where

import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax

import qualified Data.ByteString as B

import           Database.PostgreSQL.Store.Columns

-- | Lift "ByteString".
liftByteString :: B.ByteString -> Q Exp
liftByteString bs =
	[e| B.pack $(lift (B.unpack bs)) |]

-- | Implement "Column" for a type which implements "Bounded", "Enum" and "Show".
makeEnum :: Name -> B.ByteString -> Q [Dec]
makeEnum typeName name =
	[d|
		instance Column $(conT typeName) where
			pack =
				packEnumValue

			unpack =
				unpackEnumValue

			columnInfo _ =
				defaultColumnInfo {
					columnTypeName = $(liftByteString name)
				}
	|]
