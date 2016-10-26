{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, DefaultSignatures #-}

-- |
-- Module:     Database.PostgreSQL.Store.Enum
-- Copyright:  (c) Ole Krüger 2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.Enum (
	-- * Template Haskell
	createEnum,
	createEnum_
) where

import qualified Data.ByteString as B

import           Database.PostgreSQL.Store.Query
import           Database.PostgreSQL.Store.Utilities

-- | Create an enum type using the given name and values.
createEnum :: B.ByteString -> [B.ByteString] -> Query ()
createEnum name values =
	[pgsq| CREATE TYPE ${insertName name} AS ENUM (${map insertQuote values}) |]

-- | Create an enum type using the given name. This functions figures out which values the enum can
-- have using its 'Enum' and 'Bounded' instances.
createEnum_ :: (Enum a, Bounded a, Show a) => B.ByteString -> proxy a -> Query ()
createEnum_ name proxy =
	createEnum name (map showByteString values)
	where
		values = (const [minBound .. maxBound] :: (Bounded a, Enum a) => proxy a -> [a]) proxy
