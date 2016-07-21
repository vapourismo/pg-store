{-# LANGUAGE OverloadedStrings, RecordWildCards, FlexibleInstances, TemplateHaskell #-}

-- |
-- Module:     Database.PostgreSQL.Store.Columns
-- Copyright:  (c) Ole Krüger 2015-2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.Columns (
	-- *
	Value (..),

	-- *
	Column (..)
) where

import           Data.Int
import           Data.Word
import           Data.Bits
import           Data.Monoid
import           Data.Typeable
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.ByteString.Builder
import           Data.Attoparsec.ByteString
import           Data.Attoparsec.ByteString.Char8 (signed, decimal)

import           Database.PostgreSQL.LibPQ (Oid)
import qualified Database.PostgreSQL.Store.OIDs as OID

-- | Query parameter or value of a column - see 'pack' on how to generate 'Value's manually but
--   conveniently.
data Value
	= Value {
		-- | Type object identifier
		valueType :: Oid,

		-- | Data value
		valueData :: B.ByteString
	}
	| NullValue
	deriving (Show, Eq, Ord)

-- | Types which implement this type class may be used as column types.
class Column a where
	-- | Pack column value.
	pack :: a -> Value

	-- | Unpack column value.
	unpack :: Value -> Maybe a

	-- | Name of the underlying SQL type.
	columnTypeName :: Proxy a -> String

	-- | May the column be NULL?
	columnAllowNull :: Proxy a -> Bool
	columnAllowNull _proxy = False

	-- | A condition that must hold true for the column.
	columnCheck :: Proxy a -> String -> Maybe String
	columnCheck _proxy _identifier = Nothing

	-- | Generate column description in SQL. Think @CREATE TABLE@.
	columnDescription :: Proxy a -> String -> String
	columnDescription proxy identifier =
		identifier ++ " " ++
		columnTypeName proxy ++
		if columnAllowNull proxy then "" else " NOT NULL" ++
		case columnCheck proxy identifier of
			Just stmt -> " CHECK (" ++ stmt ++ ")"
			Nothing   -> ""

instance (Column a) => Column (Maybe a) where
	pack = maybe NullValue pack

	unpack NullValue = Just Nothing
	unpack val       = Just <$> unpack val

	columnTypeName proxy = columnTypeName ((const Proxy :: Proxy (Maybe b) -> Proxy b) proxy)
	columnAllowNull _    = True
	columnCheck proxy    = columnCheck ((const Proxy :: Proxy (Maybe b) -> Proxy b) proxy)

instance Column Bool where
	pack True = Value $(OID.bool) "true"
	pack _    = Value $(OID.bool) "false"

	unpack (Value $(OID.bool) "true") = Just True
	unpack (Value $(OID.bool) "TRUE") = Just True
	unpack (Value $(OID.bool) "t"   ) = Just True
	unpack (Value $(OID.bool) "y"   ) = Just True
	unpack (Value $(OID.bool) "yes" ) = Just True
	unpack (Value $(OID.bool) "YES" ) = Just True
	unpack (Value $(OID.bool) "on"  ) = Just True
	unpack (Value $(OID.bool) "ON"  ) = Just True
	unpack (Value $(OID.bool) "1"   ) = Just True
	unpack (Value $(OID.bool) _     ) = Just False
	unpack _                          = Nothing

	columnTypeName _ = "bool"

instance Column Int where
	pack n = Value $(OID.int8) (buildByteString intDec n)

	unpack (Value $(OID.int2) dat) = parseMaybe (signed decimal) dat
	unpack (Value $(OID.int4) dat) = parseMaybe (signed decimal) dat
	unpack (Value $(OID.int8) dat) = parseMaybe (signed decimal) dat
	unpack _                       = Nothing

	columnTypeName _ = "int8"

	columnCheck _ nm =
		Just (nm ++ " >= " ++ show (minBound :: Int) ++
		      " AND " ++
		      nm ++ " <= " ++ show (maxBound :: Int))

instance Column Int8 where
	pack n = Value $(OID.int2) (buildByteString int8Dec n)

	unpack (Value $(OID.int2) dat) = parseMaybe (signed decimal) dat
	unpack (Value $(OID.int4) dat) = parseMaybe (signed decimal) dat
	unpack (Value $(OID.int8) dat) = parseMaybe (signed decimal) dat
	unpack _                       = Nothing

	columnTypeName _ = "int2"

instance Column Int16 where
	pack n = Value $(OID.int2) (buildByteString int16Dec n)

	unpack (Value $(OID.int2) dat) = parseMaybe (signed decimal) dat
	unpack (Value $(OID.int4) dat) = parseMaybe (signed decimal) dat
	unpack (Value $(OID.int8) dat) = parseMaybe (signed decimal) dat
	unpack _                        = Nothing

	columnTypeName _ = "int2"

instance Column Int32 where
	pack n = Value $(OID.int4) (buildByteString int32Dec n)

	unpack (Value $(OID.int2) dat) = parseMaybe (signed decimal) dat
	unpack (Value $(OID.int4) dat) = parseMaybe (signed decimal) dat
	unpack (Value $(OID.int8) dat) = parseMaybe (signed decimal) dat
	unpack _                       = Nothing


	columnTypeName _ = "int4"

instance Column Int64 where
	pack n = Value $(OID.int8) (buildByteString int64Dec n)

	unpack (Value $(OID.int2) dat) = parseMaybe (signed decimal) dat
	unpack (Value $(OID.int4) dat) = parseMaybe (signed decimal) dat
	unpack (Value $(OID.int8) dat) = parseMaybe (signed decimal) dat
	unpack _                       = Nothing

	columnTypeName _ = "int8"

-- | Does "Word" require to be stored in type "numeric"?
wordRequiresNumeric :: Bool
wordRequiresNumeric =
	-- int8 upper bound is 2^63 - 1 (9223372036854775807)
	(fromIntegral (maxBound :: Word) :: Integer) > 9223372036854775807

instance Column Word where
	pack n | wordRequiresNumeric = Value $(OID.numeric) (buildByteString wordDec n)
	       | otherwise           = Value $(OID.int8)    (buildByteString wordDec n)

	unpack (Value $(OID.int2)    dat) = parseMaybe decimal dat
	unpack (Value $(OID.int4)    dat) = parseMaybe decimal dat
	unpack (Value $(OID.int8)    dat) = parseMaybe decimal dat
	unpack (Value $(OID.numeric) dat) = parseMaybe decimal dat
	unpack _                          = Nothing

	columnTypeName _ = if wordRequiresNumeric then "numeric(20, 0)" else "int8"

	columnCheck _ nm =
		Just (nm ++ " >= 0 AND " ++ nm ++ " <= " ++ show (maxBound :: Word))

instance Column Word8 where
	pack n = Value $(OID.int2) (buildByteString word8Dec n)

	unpack (Value $(OID.int2) dat) = parseMaybe decimal dat
	unpack (Value $(OID.int4) dat) = parseMaybe decimal dat
	unpack (Value $(OID.int8) dat) = parseMaybe decimal dat
	unpack _                       = Nothing

	columnTypeName _ = "int2"

	columnCheck _ nm =
		Just (nm ++ " >= 0 AND " ++ nm ++ " <= " ++ show (maxBound :: Word8))

instance Column Word16 where
	pack n = Value $(OID.int4) (buildByteString word16Dec n)

	unpack (Value $(OID.int2) dat) = parseMaybe decimal dat
	unpack (Value $(OID.int4) dat) = parseMaybe decimal dat
	unpack (Value $(OID.int8) dat) = parseMaybe decimal dat
	unpack _                       = Nothing

	columnTypeName _ = "int4"

	columnCheck _ nm =
		Just (nm ++ " >= 0 AND " ++ nm ++ " <= " ++ show (maxBound :: Word16))

instance Column Word32 where
	pack n = Value $(OID.int8) (buildByteString word32Dec n)

	unpack (Value $(OID.int2) dat) = parseMaybe decimal dat
	unpack (Value $(OID.int4) dat) = parseMaybe decimal dat
	unpack (Value $(OID.int8) dat) = parseMaybe decimal dat
	unpack _                       = Nothing

	columnTypeName _ = "bigint"

	columnCheck _ nm =
		Just (nm ++ " >= 0 AND " ++ nm ++ " <= " ++ show (maxBound :: Word32))

instance Column Word64 where
	pack n = Value $(OID.numeric) (buildByteString word64Dec n)

	unpack (Value $(OID.int2)    dat) = parseMaybe decimal dat
	unpack (Value $(OID.int4)    dat) = parseMaybe decimal dat
	unpack (Value $(OID.int8)    dat) = parseMaybe decimal dat
	unpack (Value $(OID.numeric) dat) = parseMaybe decimal dat
	unpack _                          = Nothing

	columnTypeName _ = "numeric(20, 0)"

	columnCheck _ nm =
		Just (nm ++ " >= 0 AND " ++ nm ++ " <= " ++ show (maxBound :: Word64))

instance Column Integer where
	pack n = Value $(OID.numeric) (buildByteString integerDec n)

	unpack (Value $(OID.int2)    dat) = parseMaybe decimal dat
	unpack (Value $(OID.int4)    dat) = parseMaybe decimal dat
	unpack (Value $(OID.int8)    dat) = parseMaybe decimal dat
	unpack (Value $(OID.numeric) dat) = parseMaybe decimal dat
	unpack _                          = Nothing

	columnTypeName _ = "numeric"

instance Column [Char] where
	pack str = Value $(OID.text) (buildByteString stringUtf8 str)

	unpack (Value $(OID.varchar) dat) = pure (T.unpack (T.decodeUtf8 dat))
	unpack (Value $(OID.char)    dat) = pure (T.unpack (T.decodeUtf8 dat))
	unpack (Value $(OID.text)    dat) = pure (T.unpack (T.decodeUtf8 dat))
	unpack _                          = Nothing

	columnTypeName _ = "text"

instance Column T.Text where
	pack txt = Value $(OID.text) (T.encodeUtf8 txt)

	unpack (Value $(OID.varchar) dat) = pure (T.decodeUtf8 dat)
	unpack (Value $(OID.char)    dat) = pure (T.decodeUtf8 dat)
	unpack (Value $(OID.text)    dat) = pure (T.decodeUtf8 dat)
	unpack _                          = Nothing

	columnTypeName _ = "text"

instance Column TL.Text where
	pack txt = pack (TL.toStrict txt)

	unpack val = TL.fromStrict <$> unpack val

	columnTypeName _  = columnTypeName (Proxy :: Proxy T.Text)
	columnAllowNull _ = columnAllowNull (Proxy :: Proxy T.Text)
	columnCheck _     = columnCheck (Proxy :: Proxy T.Text)

instance Column B.ByteString where
	pack bs = Value $(OID.bytea) (encodeByteaHex bs)

	unpack (Value $(OID.varchar) dat) = pure dat
	unpack (Value $(OID.char)    dat) = pure dat
	unpack (Value $(OID.text)    dat) = pure dat
	unpack (Value $(OID.bytea)   dat) = decodeByteaHex dat
	unpack _                          = Nothing

	columnTypeName _ = "bytea"

instance Column BL.ByteString where
	pack bs = pack (BL.toStrict bs)

	unpack val = BL.fromStrict <$> unpack val

	columnTypeName _  = columnTypeName (Proxy :: Proxy B.ByteString)
	columnAllowNull _ = columnAllowNull (Proxy :: Proxy B.ByteString)
	columnCheck _     = columnCheck (Proxy :: Proxy B.ByteString)

-- | Produce the two-digit hexadecimal representation of a 8-bit word.
word8ToHex :: Word8 -> B.ByteString
word8ToHex w =
	hex (shiftR w 4) <> hex (w .&. 15)
	where
		hex n =
			-- lel
			case n of {
				15 -> "F"; 14 -> "E"; 13 -> "D"; 12 -> "C"; 11 -> "B";
				10 -> "A"; 9  -> "9"; 8  -> "8"; 7  -> "7"; 6  -> "6";
				5  -> "5"; 4  -> "4"; 3  -> "3"; 2  -> "2"; 1  -> "1";
				_  -> "0"
			}

-- | Retrieve 8-bit word from two-digit hexadecimal representation.
hexToWord8 :: B.ByteString -> Word8
hexToWord8 bs =
	case B.unpack bs of
		(a : b : _) -> shiftL (unhex a) 4 .|. unhex b
		(a : _) -> unhex a
		_ -> 0
	where
		unhex n =
			-- double lel
			case n of {
				48  ->  0; 49  ->  1; 50 ->  2; 51 ->  3; 52  ->  4;
				53  ->  5; 54  ->  6; 55 ->  7; 56 ->  8; 57  ->  9;
				65  -> 10; 66  -> 11; 67 -> 12; 68 -> 13; 69  -> 14;
				70  -> 15; 97  -> 10; 98 -> 11; 99 -> 12; 100 -> 13;
				101 -> 14; 102 -> 15; _  ->  0
			}

-- | Unpack a byte array in textual representation.
decodeByteaHex :: B.ByteString -> Maybe B.ByteString
decodeByteaHex bs
	| B.length bs >= 2 && mod (B.length bs) 2 == 0 && B.isPrefixOf "\\x" bs =
		Just (B.pack (unfoldHex (B.drop 2 bs)))
	| otherwise = Nothing
		where
			unfoldHex "" = []
			unfoldHex bs = hexToWord8 (B.take 2 bs) : unfoldHex (B.drop 2 bs)

-- | Pack textual representation of a byte array.
encodeByteaHex :: B.ByteString -> B.ByteString
encodeByteaHex bs =
	"\\x" <> B.concatMap word8ToHex bs

-- | Finish the parsing process.
finishParser :: Result r -> Result r
finishParser (Partial f) = f B.empty
finishParser x = x

-- | Parse a ByteString.
parseMaybe :: Parser a -> B.ByteString -> Maybe a
parseMaybe p i =
	maybeResult (finishParser (parse p i))

-- | Build strict ByteString.
buildByteString :: (a -> Builder) -> a -> B.ByteString
buildByteString f x =
	BL.toStrict (toLazyByteString (f x))
