{-# LANGUAGE TemplateHaskell, QuasiQuotes, RankNTypes #-}

-- |
-- Module:     Database.PostgreSQL.Store.OIDs
-- Copyright:  (c) Ole Krüger 2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.OIDs (
	OIDQ,

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

class GenOID a where
	genOID :: Integer -> Q a

instance GenOID Exp where
	genOID oid = [e| P.Oid $(litE (IntegerL oid)) |]

instance GenOID Pat where
	genOID oid = [p| P.Oid $(pure (LitP (IntegerL oid))) |]

-- | A type which can be coerced into @Q Exp@ or @Q Pat@.
type OIDQ = forall a . GenOID a => Q a

-- | Boolean
bool :: OIDQ
bool = genOID 16

-- | 16-bit integer
int2 :: OIDQ
int2 = genOID 21

-- | 32-bit integer
int4 :: OIDQ
int4 = genOID 23

-- | 64-bit integer
int8 :: OIDQ
int8 = genOID 20

-- | Single-precision floating-point number
float4 :: OIDQ
float4 = genOID 700

-- | Double-precision floating-point number
float8 :: OIDQ
float8 = genOID 701

-- | Arbitrary precision number
numeric :: OIDQ
numeric = genOID 1700

-- | Fixed-length string
char :: OIDQ
char    = genOID 1042

-- | Variable-length string
varchar :: OIDQ
varchar = genOID 1043

-- | Unlimited variable-length string
text :: OIDQ
text = genOID 25

-- | Byte array
bytea :: OIDQ
bytea = genOID 17
