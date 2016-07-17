{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

-- |
-- Module:     Database.PostgreSQL.Store.OIDs
-- Copyright:  (c) Ole Krüger 2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.OIDs (
	bool,
	int2,
	int4,
	int8,
	float4,
	float8,
	numeric,
	char,
	varchar,
	text,
	bytea
) where

import           Language.Haskell.TH
import qualified Database.PostgreSQL.LibPQ as P

class IntegerToQOID a where
	toQOID :: Integer -> Q a

instance IntegerToQOID Exp where
	toQOID oid = [e| P.Oid $(litE (IntegerL oid)) |]

instance IntegerToQOID Pat where
	toQOID oid = [p| P.Oid $(pure (LitP (IntegerL oid))) |]

-- | Boolean
bool :: (IntegerToQOID a) => Q a
bool = toQOID 16

-- | 16-bit integer
int2 :: (IntegerToQOID a) => Q a
int2 = toQOID 21

-- | 32-bit integer
int4 :: (IntegerToQOID a) => Q a
int4 = toQOID 23

-- | 64-bit integer
int8 :: (IntegerToQOID a) => Q a
int8 = toQOID 20

-- | Single-precision floating-point number
float4 :: (IntegerToQOID a) => Q a
float4 = toQOID 700

-- | Double-precision floating-point number
float8 :: (IntegerToQOID a) => Q a
float8 = toQOID 701

-- | Arbitrary precision number
numeric :: (IntegerToQOID a) => Q a
numeric = toQOID 1700

-- | Fixed-length string
char :: (IntegerToQOID a) => Q a
char    = toQOID 1042

-- | Variable-length string
varchar :: (IntegerToQOID a) => Q a
varchar = toQOID 1043

-- | Unlimited variable-length string
text :: (IntegerToQOID a) => Q a
text = toQOID 25

-- | Byte array
bytea :: (IntegerToQOID a) => Q a
bytea = toQOID 17
