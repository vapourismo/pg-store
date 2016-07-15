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
	Column (..),
	makeColumnDescription
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

import qualified Database.PostgreSQL.LibPQ as P
import           Database.PostgreSQL.Store.OIDs

-- | Query parameter or value of a column - see 'pack' on how to generate 'Value's manually but
--   conveniently.
data Value
	= Value {
		-- | Type object identifier
		valueType :: P.Oid,

		-- | Data value
		valueData :: B.ByteString,

		-- | Data format
		valueFormat :: P.Format
	}
	| NullValue
	deriving (Show, Eq, Ord)

-- | Column type
class Column a where
	-- | Pack column value.
	pack :: a -> Value

	-- | Unpack column value.
	unpack :: Value -> Maybe a

	-- | Name of the underlying type.
	columnTypeName :: Proxy a -> String

	-- | May the type be NULL?
	columnAllowNull :: Proxy a -> Bool

-- | Generate column description in SQL. Think @CREATE TABLE@.
makeColumnDescription :: (Column a) => Proxy a -> String
makeColumnDescription proxy =
	columnTypeName proxy ++ (if columnAllowNull proxy then "" else " NOT NULL")

instance (Column a) => Column (Maybe a) where
	pack = maybe NullValue pack

	unpack NullValue = Just Nothing
	unpack val       = fmap Just (unpack val)

	columnTypeName proxy = columnTypeName ((const Proxy :: Proxy (Maybe b) -> Proxy b) proxy)
	columnAllowNull _    = True

instance Column Bool where
	pack v =
		Value {
			valueType   = $boolOID,
			valueData   = if v then "true" else "false",
			valueFormat = P.Text
		}

	unpack (Value $boolOID "true" P.Text) = Just True
	unpack (Value $boolOID "TRUE" P.Text) = Just True
	unpack (Value $boolOID "t"    P.Text) = Just True
	unpack (Value $boolOID "y"    P.Text) = Just True
	unpack (Value $boolOID "yes"  P.Text) = Just True
	unpack (Value $boolOID "YES"  P.Text) = Just True
	unpack (Value $boolOID "on"   P.Text) = Just True
	unpack (Value $boolOID "ON"   P.Text) = Just True
	unpack (Value $boolOID "1"    P.Text) = Just True
	unpack (Value $boolOID _      P.Text) = Just False
	unpack _                              = Nothing

	columnTypeName  _ = "bool"
	columnAllowNull _ = False

instance Column Int where
	pack n =
		Value {
			valueType   = $integerOID,
			valueData   = buildByteString intDec n,
			valueFormat = P.Text
		}

	unpack (Value $bigintOID   dat P.Text) = parseMaybe (signed decimal) dat
	unpack (Value $smallintOID dat P.Text) = parseMaybe (signed decimal) dat
	unpack (Value $integerOID  dat P.Text) = parseMaybe (signed decimal) dat
	unpack _                               = Nothing

	columnTypeName _  = "integer"
	columnAllowNull _ = False

instance Column Int8 where
	pack n =
		Value {
			valueType   = $smallintOID,
			valueData   = buildByteString int8Dec n,
			valueFormat = P.Text
		}

	unpack (Value $bigintOID   dat P.Text) = parseMaybe (signed decimal) dat
	unpack (Value $smallintOID dat P.Text) = parseMaybe (signed decimal) dat
	unpack (Value $integerOID  dat P.Text) = parseMaybe (signed decimal) dat
	unpack _                               = Nothing

	columnTypeName _  = "smallint"
	columnAllowNull _ = False

instance Column Int16 where
	pack n =
		Value {
			valueType   = $smallintOID,
			valueData   = buildByteString int16Dec n,
			valueFormat = P.Text
		}

	unpack (Value $bigintOID   dat P.Text) = parseMaybe (signed decimal) dat
	unpack (Value $smallintOID dat P.Text) = parseMaybe (signed decimal) dat
	unpack (Value $integerOID  dat P.Text) = parseMaybe (signed decimal) dat
	unpack _                               = Nothing

	columnTypeName _  = "smallint"
	columnAllowNull _ = False

instance Column Int32 where
	pack n =
		Value {
			valueType   = $integerOID,
			valueData   = buildByteString int32Dec n,
			valueFormat = P.Text
		}

	unpack (Value $bigintOID   dat P.Text) = parseMaybe (signed decimal) dat
	unpack (Value $smallintOID dat P.Text) = parseMaybe (signed decimal) dat
	unpack (Value $integerOID  dat P.Text) = parseMaybe (signed decimal) dat
	unpack _                               = Nothing


	columnTypeName _  = "integer"
	columnAllowNull _ = False

instance Column Int64 where
	pack n =
		Value {
			valueType   = $bigintOID,
			valueData   = buildByteString int64Dec n,
			valueFormat = P.Text
		}

	unpack (Value $bigintOID   dat P.Text) = parseMaybe (signed decimal) dat
	unpack (Value $smallintOID dat P.Text) = parseMaybe (signed decimal) dat
	unpack (Value $integerOID  dat P.Text) = parseMaybe (signed decimal) dat
	unpack _                               = Nothing

	columnTypeName _  = "bigint"
	columnAllowNull _ = False

instance Column [Char] where
	pack str =
		Value {
			valueType   = $textOID,
			valueData   = buildByteString stringUtf8 str,
			valueFormat = P.Text
		}

	unpack (Value $varcharOID dat P.Text) = pure (T.unpack (T.decodeUtf8 dat))
	unpack (Value $charOID    dat P.Text) = pure (T.unpack (T.decodeUtf8 dat))
	unpack (Value $textOID    dat P.Text) = pure (T.unpack (T.decodeUtf8 dat))
	unpack _                              = Nothing

	columnTypeName _  = "text"
	columnAllowNull _ = False

instance Column T.Text where
	pack txt =
		Value {
			valueType   = $textOID,
			valueData   = T.encodeUtf8 txt,
			valueFormat = P.Text
		}

	unpack (Value $varcharOID dat P.Text) = pure (T.decodeUtf8 dat)
	unpack (Value $charOID    dat P.Text) = pure (T.decodeUtf8 dat)
	unpack (Value $textOID    dat P.Text) = pure (T.decodeUtf8 dat)
	unpack _                              = Nothing

	columnTypeName _  = "text"
	columnAllowNull _ = False

instance Column TL.Text where
	pack txt =
		pack (TL.toStrict txt)

	unpack val =
		TL.fromStrict <$> unpack val

	columnTypeName _  = columnTypeName (Proxy :: Proxy T.Text)
	columnAllowNull _ = columnAllowNull (Proxy :: Proxy T.Text)

instance Column B.ByteString where
	pack bs =
		Value {
			valueType   = $byteaOID,
			valueData   = toTextByteArray bs,
			valueFormat = P.Text
		}

	unpack (Value $varcharOID dat P.Text) = pure dat
	unpack (Value $charOID    dat P.Text) = pure dat
	unpack (Value $textOID    dat P.Text) = pure dat
	unpack (Value $byteaOID   dat P.Text) = fromTextByteArray dat
	unpack _                              = Nothing

	columnTypeName _  = "bytea"
	columnAllowNull _ = False

instance Column BL.ByteString where
	pack bs =
		pack (BL.toStrict bs)

	unpack val =
		BL.fromStrict <$> unpack val

	columnTypeName _  = columnTypeName (Proxy :: Proxy B.ByteString)
	columnAllowNull _ = columnAllowNull (Proxy :: Proxy B.ByteString)

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
fromTextByteArray :: B.ByteString -> Maybe B.ByteString
fromTextByteArray bs
	| B.length bs >= 2 && mod (B.length bs) 2 == 0 && B.isPrefixOf "\\x" bs =
		Just (B.pack (unfoldHex (B.drop 2 bs)))
	| otherwise = Nothing
		where
			unfoldHex "" = []
			unfoldHex bs = hexToWord8 (B.take 2 bs) : unfoldHex (B.drop 2 bs)

-- | Pack textual representation of a byte array.
toTextByteArray :: B.ByteString -> B.ByteString
toTextByteArray bs =
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
