{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

-- |
-- Module:     Database.PostgreSQL.Store.Enum
-- Copyright:  (c) Ole Krüger 2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.Enum (
	-- * Helpers
	EnumWrapper (..),
	toEnumValue,
	packEnumValue,
	unpackEnumValue,

	-- * Template Haskell
	makeEnum
) where

import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax

import           Data.List
import qualified Data.ByteString    as B
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

import           Database.PostgreSQL.LibPQ (invalidOid)
import           Database.PostgreSQL.Store.Columns

-- | Wrapper for enumeration types.
newtype EnumWrapper a = EnumWrapper { fromEnumWrapper :: a }

-- | Try to find the enum value based on the input string.
toEnumValue :: (Enum a, Bounded a, Show a) => String -> Maybe a
toEnumValue =
	findValue [minBound .. maxBound]
	where
		findValue :: (Enum a, Show a) => [a] -> String -> Maybe a
		findValue values value =
			toEnum <$> elemIndex value (map show values)

-- | Pack an instance of an "Enum".
packEnumValue :: (Show a) => a -> Value
packEnumValue value =
	(pack (show value)) {valueType = invalidOid}

-- | Unpack an instance of an "Enum".
unpackEnumValue :: (Enum a, Bounded a, Show a) => Value -> Maybe a
unpackEnumValue (Value _ dat) = toEnumValue (T.unpack (T.decodeUtf8 dat))
unpackEnumValue	_             = Nothing

instance (Enum a, Bounded a, Show a) => Column (EnumWrapper a) where
	pack (EnumWrapper value) =
		packEnumValue value

	unpack value =
		EnumWrapper <$> unpackEnumValue value

	columnInfo _ =
		defaultColumnInfo {
			columnTypeName = "text"
		}

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
