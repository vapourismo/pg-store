{-# LANGUAGE OverloadedStrings, TemplateHaskell, RecordWildCards, ExistentialQuantification,
             StandaloneDeriving #-}

module Database.PostgreSQL.Store.Types where

import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Except

import           Data.Int
import           Data.Word
import           Data.Bits
import           Data.Monoid
import           Data.Typeable
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.ByteString.Builder
import           Data.Attoparsec.ByteString
import           Data.Attoparsec.ByteString.Char8 (signed, decimal)

import qualified Database.PostgreSQL.LibPQ as P

-- | Query parameter or value of a column
data Value = Value {
	-- | Type OID
	valueType :: P.Oid,

	-- | Data value
	valueData :: B.ByteString,

	-- | Data format
	valueFormat :: P.Format
} deriving (Show, Eq, Ord)

-- | Query including statement and parameters.
data Query = Query {
	-- | Statement
	queryStatement :: B.ByteString,

	-- | Parameters
	queryParams :: [Value]
} deriving (Show, Eq, Ord)

-- | Description of a column type
data ColumnDescription a = ColumnDescription {
	-- | Type name (e.g. bool, integer)
	columnTypeName :: String,

	-- | Can the column be null?
	columnTypeNull :: Bool
} deriving (Show, Eq, Ord)

-- | Error that occured during result processing
data ResultError
	= UnknownColumnName B.ByteString
	| ColumnDataMissing P.Row P.Column
	| forall a. ValueError P.Row P.Column P.Oid P.Format (ColumnDescription a)

deriving instance Show ResultError

-- | Result processor
type ResultProcessor = ReaderT P.Result (ExceptT ResultError IO)

-- | Description of a table type
data TableDescription a = TableDescription {
	-- | Table name
	tableName :: String,

	-- | Identifer column name
	tableIdentColumn :: String
} deriving (Show, Eq, Ord)

-- | Table type
class Table a where
	-- | Generate an INSERT query which adds a row to the table and returns its identifier.
	insertQuery :: a -> Query

	-- | Generate an UPDATE query which updates an existing row.
	updateQuery :: Row a -> Query

	-- | Generate a DELETE query which removes an existing row.
	deleteQuery :: (HasID i) => i a -> Query

	-- | Generate a CREATE query which creates the table.
	createQuery :: Proxy a -> Query

	-- | Extract rows of this table from the given result.
	tableResultProcessor :: ResultProcessor [Row a]

	-- | Describe the table.
	tableDescription :: TableDescription a


-- | Result row
class ResultRow a where
	-- | Extract rows from the given result.
	fromResult :: ResultProcessor [a]

-- | A value of that type contains an ID.
class HasID a where
	-- | Retrieve the underlying ID.
	referenceID :: a b -> Int64

-- | Column type
class Column a where
	-- | Pack column value.
	pack :: a -> Value

	-- | Unpack column value.
	unpack :: Value -> Maybe a

	-- | Descripe the column.
	columnDescription :: ColumnDescription a

instance Column Bool where
	pack v =
		Value {
			valueType   = P.Oid 16,
			valueData   = if v then "true" else "false",
			valueFormat = P.Text
		}

	unpack (Value (P.Oid 16) "true"  P.Text) = Just True
	unpack (Value (P.Oid 16) "false" P.Text) = Just False
	unpack _                                 = Nothing

	columnDescription =
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

	columnDescription =
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

	columnDescription =
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

	columnDescription =
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


	columnDescription =
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

	columnDescription =
		ColumnDescription {
			columnTypeName = "bigint",
			columnTypeNull = False
		}

instance Column T.Text where
	pack txt =
		Value {
			valueType   = P.Oid 25,
			valueData   = T.encodeUtf8 txt,
			valueFormat = P.Text
		}

	unpack (Value (P.Oid 16)   dat P.Text) = Just (T.decodeUtf8 dat)
	unpack (Value (P.Oid 25)   dat P.Text) = Just (T.decodeUtf8 dat)
	unpack (Value (P.Oid 1043) dat P.Text) = Just (T.decodeUtf8 dat)
	unpack _                               = Nothing

	columnDescription =
		ColumnDescription {
			columnTypeName = "text",
			columnTypeNull = False
		}

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

	columnDescription =
		ColumnDescription {
			columnTypeName = "bytea",
			columnTypeNull = False
		}

instance Column BL.ByteString where
	pack = pack . BL.toStrict

	unpack (Value (P.Oid 17) dat P.Binary) = pure (BL.fromStrict dat)
	unpack (Value (P.Oid 17) dat P.Text)   = BL.fromStrict <$> fromTextByteArray dat
	unpack _                               = Nothing

	columnDescription =
		coerceColumnDescription (columnDescription :: ColumnDescription B.ByteString)

-- | Resolved row
data Row a = Row Int64 a
	deriving (Show, Eq, Ord)

instance HasID Row where
	referenceID (Row rid _) = rid

instance (Table a) => ResultRow (Row a) where
	fromResult = tableResultProcessor

-- | Reference to a row or the resolved row
data Reference a = Reference Int64 | Resolved (Row a)
	deriving (Show, Eq, Ord)

instance HasID Reference where
	referenceID (Reference rid) = rid
	referenceID (Resolved row)  = referenceID row

instance (Table a) => Column (Reference a) where
	pack ref = pack (referenceID ref)
	unpack val = Reference <$> unpack val

	columnDescription =
		make tableDescription
		where
			make :: TableDescription a -> ColumnDescription (Reference a)
			make TableDescription {..} =
				ColumnDescription {
					columnTypeName = "bigint references " ++ tableName ++ " (" ++ tableIdentColumn ++ ")",
					columnTypeNull = False
				}

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

-- | Convert between to column descriptions
coerceColumnDescription :: ColumnDescription a -> ColumnDescription b
coerceColumnDescription ColumnDescription {..} =
	ColumnDescription {
		columnTypeName = columnTypeName,
		columnTypeNull = columnTypeNull
	}

-- | Generate SQL column description.
makeColumnDescription :: ColumnDescription a -> String
makeColumnDescription ColumnDescription {..} =
	columnTypeName ++ (if columnTypeNull then "" else " NOT NULL")
