{-# LANGUAGE OverloadedStrings, RecordWildCards, FlexibleInstances #-}

module Database.PostgreSQL.Store.Columns (
	-- *
	Value (..),

	-- *
	ColumnDescription (..),
	makeColumnDescription,

	-- *
	sanitizeName,
	sanitizeName',
	identField,
	identField',

	-- *
	Column (..),
) where

import           Language.Haskell.TH

import           Data.Int
import           Data.Word
import           Data.Bits
import           Data.Monoid
import           Data.Typeable
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.ByteString.Builder
import           Data.Attoparsec.ByteString
import           Data.Attoparsec.ByteString.Char8 (signed, decimal)

import qualified Database.PostgreSQL.LibPQ as P

-- | Query parameter or value of a column
data Value
	= Value {
		-- | Type OID
		valueType :: P.Oid,

		-- | Data value
		valueData :: B.ByteString,

		-- | Data format
		valueFormat :: P.Format
	}
	| NullValue
	deriving (Show, Eq, Ord)

-- | Description of a column type
data ColumnDescription = ColumnDescription {
	-- | Type name (e.g. bool, integer)
	columnTypeName :: String,

	-- | Can the column be null?
	columnTypeNull :: Bool
} deriving (Show, Eq, Ord)

-- | Generate SQL column description.
makeColumnDescription :: ColumnDescription -> String
makeColumnDescription ColumnDescription {..} =
	columnTypeName ++ (if columnTypeNull then "" else " NOT NULL")

-- | Generate the sanitized representation of a name.
sanitizeName :: Name -> String
sanitizeName name =
	show name

-- | Similiar to "sanitizeName" but encloses the name in quotes.
sanitizeName' :: Name -> String
sanitizeName' name =
	"\"" ++ sanitizeName name ++ "\""

-- | Generate the name of the identifying field.
identField :: Name -> String
identField name =
	show name ++ "$id"

-- | Similiar to "identField" but encloses the name in quotes.
identField' :: Name -> String
identField' name =
	"\"" ++ identField name ++ "\""

-- | Column type
class Column a where
	-- | Pack column value.
	pack :: a -> Value

	-- | Unpack column value.
	unpack :: Value -> Maybe a

	-- | Descripe the column.
	describeColumn :: Proxy a -> ColumnDescription

instance Column Bool where
	pack v =
		Value {
			valueType   = P.Oid 16,
			valueData   = if v then "true" else "false",
			valueFormat = P.Text
		}

	unpack (Value (P.Oid 16) "true" P.Text) = Just True
	unpack (Value (P.Oid 16) "TRUE" P.Text) = Just True
	unpack (Value (P.Oid 16) "t"    P.Text) = Just True
	unpack (Value (P.Oid 16) "y"    P.Text) = Just True
	unpack (Value (P.Oid 16) "yes"  P.Text) = Just True
	unpack (Value (P.Oid 16) "on"   P.Text) = Just True
	unpack (Value (P.Oid 16) "1"    P.Text) = Just True
	unpack (Value (P.Oid 16) _      P.Text) = Just False
	unpack _                                = Nothing

	describeColumn _ =
		ColumnDescription {
			columnTypeName = "bool",
			columnTypeNull = False
		}

instance Column Int where
	pack n =
		Value {
			valueType   = P.Oid 23,
			valueData   = buildByteString intDec n,
			valueFormat = P.Text
		}

	unpack (Value (P.Oid 21) dat P.Text) = parseMaybe (signed decimal) dat
	unpack (Value (P.Oid 23) dat P.Text) = parseMaybe (signed decimal) dat
	unpack _                             = Nothing

	describeColumn _ =
		ColumnDescription {
			columnTypeName = "integer",
			columnTypeNull = False
		}

instance Column Int8 where
	pack n =
		Value {
			valueType   = P.Oid 21,
			valueData   = buildByteString int8Dec n,
			valueFormat = P.Text
		}

	unpack (Value (P.Oid 21) dat P.Text) = parseMaybe (signed decimal) dat
	unpack _                             = Nothing

	describeColumn _ =
		ColumnDescription {
			columnTypeName = "smallint",
			columnTypeNull = False
		}

instance Column Int16 where
	pack n =
		Value {
			valueType   = P.Oid 21,
			valueData   = buildByteString int16Dec n,
			valueFormat = P.Text
		}

	unpack (Value (P.Oid 21) dat P.Text) = parseMaybe (signed decimal) dat
	unpack _                             = Nothing

	describeColumn _ =
		ColumnDescription {
			columnTypeName = "smallint",
			columnTypeNull = False
		}

instance Column Int32 where
	pack n =
		Value {
			valueType   = P.Oid 23,
			valueData   = buildByteString int32Dec n,
			valueFormat = P.Text
		}

	unpack (Value (P.Oid 21) dat P.Text) = parseMaybe (signed decimal) dat
	unpack (Value (P.Oid 23) dat P.Text) = parseMaybe (signed decimal) dat
	unpack _                             = Nothing


	describeColumn _ =
		ColumnDescription {
			columnTypeName = "integer",
			columnTypeNull = False
		}

instance Column Int64 where
	pack n =
		Value {
			valueType   = P.Oid 20,
			valueData   = buildByteString int64Dec n,
			valueFormat = P.Text
		}

	unpack (Value (P.Oid 20) dat P.Text) = parseMaybe (signed decimal) dat
	unpack (Value (P.Oid 21) dat P.Text) = parseMaybe (signed decimal) dat
	unpack (Value (P.Oid 23) dat P.Text) = parseMaybe (signed decimal) dat
	unpack _                             = Nothing

	describeColumn _ =
		ColumnDescription {
			columnTypeName = "bigint",
			columnTypeNull = False
		}

instance Column [Char] where
	pack str =
		pack (buildByteString stringUtf8 str)

	unpack val =
		T.unpack . T.decodeUtf8 <$> unpack val

	describeColumn _ =
		describeColumn (Proxy :: Proxy B.ByteString)

instance Column T.Text where
	pack txt =
		pack (T.encodeUtf8 txt)

	unpack val =
		T.decodeUtf8 <$> unpack val

	describeColumn _ =
		describeColumn (Proxy :: Proxy B.ByteString)

instance Column TL.Text where
	pack txt =
		pack (TL.encodeUtf8 txt)

	unpack val =
		TL.decodeUtf8 <$> unpack val

	describeColumn _ =
		describeColumn (Proxy :: Proxy BL.ByteString)

instance Column B.ByteString where
	pack bs =
		Value {
			valueType   = P.Oid 17,
			valueData   = toTextByteArray bs,
			valueFormat = P.Text
		}

	unpack (Value (P.Oid 17) dat P.Binary) = pure dat
	unpack (Value (P.Oid 17) dat P.Text)   = fromTextByteArray dat
	unpack _                               = Nothing

	describeColumn _ =
		ColumnDescription {
			columnTypeName = "bytea",
			columnTypeNull = False
		}

instance Column BL.ByteString where
	pack bs =
		pack (BL.toStrict bs)

	unpack val =
		BL.fromStrict <$> unpack val

	describeColumn _ =
		describeColumn (Proxy :: Proxy B.ByteString)

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
