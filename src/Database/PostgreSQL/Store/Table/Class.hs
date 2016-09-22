{-# LANGUAGE TemplateHaskell, BangPatterns, DeriveLift #-}

-- |
-- Module:     Database.PostgreSQL.Store.Table.Class
-- Copyright:  (c) Ole Krüger 2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.Table.Class (
	TableInformation (..),
	Table (..)
) where

import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax

import qualified Data.ByteString as B

-- | Table-related information about a type.
data TableInformation = TableInformation {
	tableIdentColumn :: !B.ByteString,
	tableColumns     :: ![B.ByteString]
} deriving (Show, Eq, Ord)

-- | Lift "ByteString".
liftByteString :: B.ByteString -> Q Exp
liftByteString bs =
	[e| B.pack $(lift (B.unpack bs)) |]

instance Lift TableInformation where
	lift (TableInformation identColumn columns) =
		[e| TableInformation
		        $(liftByteString identColumn)
		        $(listE (map liftByteString columns)) |]

-- | Attach table information to a type.
class Table a where
	tableInfo :: proxy a -> TableInformation
