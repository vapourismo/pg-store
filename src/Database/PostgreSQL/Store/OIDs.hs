{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

-- |
-- Module:     Database.PostgreSQL.Store.OIDs
-- Copyright:  (c) Ole Krüger 2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.OIDs where

import           Language.Haskell.TH
import qualified Database.PostgreSQL.LibPQ as P

class IntegerToQOID a where
	toQOID :: Integer -> Q a

instance IntegerToQOID Exp where
	toQOID oid = [e| P.Oid $(litE (IntegerL oid)) |]

instance IntegerToQOID Pat where
	toQOID oid = [p| P.Oid $(pure (LitP (IntegerL oid))) |]

boolOID, smallintOID, integerOID, bigintOID, numericOID, realOID, doublePrecisionOID, varcharOID, charOID, textOID, byteaOID :: (IntegerToQOID a) => Q a

boolOID            = toQOID 16
smallintOID        = toQOID 21
integerOID         = toQOID 23
bigintOID          = toQOID 20
numericOID         = toQOID 1700
realOID            = toQOID 700
doublePrecisionOID = toQOID 701
varcharOID         = toQOID 1043
charOID            = toQOID 1042
textOID            = toQOID 25
byteaOID           = toQOID 17
