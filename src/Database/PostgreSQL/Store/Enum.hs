{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, DefaultSignatures #-}

-- |
-- Module:     Database.PostgreSQL.Store.Enum
-- Copyright:  (c) Ole Krüger 2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.Enum (
	-- * Template Haskell
	createEnum
) where

import qualified Data.ByteString as B

import           Database.PostgreSQL.Store.Query
import           Database.PostgreSQL.Store.Utilities

-- | Create the query which create the enum type @a@.
createEnum :: (Enum a, Bounded a, Show a) => proxy a -> B.ByteString -> Query ()
createEnum proxy name =
	create proxy [minBound .. maxBound]
	where
		create :: (Show a) => proxy a -> [a] -> Query ()
		create _ values =
			[pgsq| CREATE TYPE ${insertName name} AS
			       ENUM (${map (insertQuote . showByteString) values}) |]
