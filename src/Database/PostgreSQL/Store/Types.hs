{-# LANGUAGE OverloadedStrings, TemplateHaskell, RecordWildCards #-}

module Database.PostgreSQL.Store.Types where

import           Control.Monad.Trans.Maybe
import           Data.Int
import           Data.Word
import           Data.Bits
import           Data.Monoid
import           Data.String
import           Data.Typeable
import qualified Data.ByteString           as B
import qualified Database.PostgreSQL.LibPQ as P

data Value = Value {
	valueType   :: P.Oid,
	valueData   :: B.ByteString,
	valueFormat :: P.Format
} deriving Show

data Statement = Statement {
	statementContent :: B.ByteString,
	statementParams  :: [Value]
} deriving Show

-- | Description of a column type
data ColumnTypeDescription a = ColumnTypeDescription {
	-- | Type name (e.g. bool, integer)
	columnTypeName :: String,

	-- | Can the column be null?
	columnTypeNull :: Bool
}

instance Show (ColumnTypeDescription a) where
	show ColumnTypeDescription {..} =
		columnTypeName ++ (if columnTypeNull then "" else " NOT NULL")

class ColumnType a where
	pack   :: a -> Value
	unpack :: Value -> Maybe a

	columnTypeDescription :: ColumnTypeDescription a

bsToIntegral :: (Integral a) => B.ByteString -> a
bsToIntegral bs =
	fromIntegral (B.foldl' reducer (0 :: Integer) bs)
	where
		reducer a b
			| b >= 48 && b <= 57 = a * 10 + fromIntegral (b - 48)
			| otherwise = a

instance ColumnType Int where
	pack n =
		Value {
			valueType   = P.Oid 23,
			valueData   = fromString (show n),
			valueFormat = P.Text
		}

	unpack (Value (P.Oid 23) dat P.Text) =
		Just (bsToIntegral dat)

	unpack _ =
		Nothing

	columnTypeDescription =
		ColumnTypeDescription {
			columnTypeName = "integer",
			columnTypeNull = False
		}

instance ColumnType Int64 where
	pack n =
		Value {
			valueType   = P.Oid 20,
			valueData   = fromString (show n),
			valueFormat = P.Text
		}

	unpack (Value (P.Oid 20) dat P.Text) =
		Just (bsToIntegral dat)

	unpack _ = Nothing

	columnTypeDescription =
		ColumnTypeDescription {
			columnTypeName = "bigint",
			columnTypeNull = False
		}

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

unescapeBytea :: B.ByteString -> Maybe B.ByteString
unescapeBytea bs
	| B.length bs >= 2 && mod (B.length bs) 2 == 0 &&
	  B.isPrefixOf "\\x" bs = Just (B.pack (unfoldHex (B.drop 2 bs)))
	| otherwise                                    = Nothing
		where
			unfoldHex "" = []
			unfoldHex bs = hexToWord8 (B.take 2 bs) : unfoldHex (B.drop 2 bs)

instance ColumnType B.ByteString where
	pack bs =
		Value {
			valueType   = P.Oid 17,
			valueData   = hex,
			valueFormat = P.Text
		}
		where
			hex = "\\x" <> B.concatMap word8ToHex bs

	unpack (Value (P.Oid 17) dat P.Binary) =
		Just dat

	unpack (Value (P.Oid 17) dat P.Text) =
		unescapeBytea dat

	unpack _ = Nothing

	columnTypeDescription =
		ColumnTypeDescription {
			columnTypeName = "bytea",
			columnTypeNull = False
		}

data Reference a = Reference Int64 | Resolved Int64 a
	deriving (Show, Eq, Ord)

referenceID :: Reference a -> Int64
referenceID (Reference rid)  = rid
referenceID (Resolved rid _) = rid

class Table a where
	insertStatement :: a -> Statement
	updateStatement :: Reference a -> Statement
	deleteStatement :: Reference a -> Statement
	createStatement :: Proxy a -> Statement
	dropStatement   :: Proxy a -> Statement
	fromResult      :: P.Result -> MaybeT IO [Reference a]
