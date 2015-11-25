{-# LANGUAGE OverloadedStrings, TemplateHaskell, RecordWildCards #-}

module Database.PostgreSQL.Store.Types where

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

instance ColumnType Int where
	pack n =
		Value {
			valueType   = P.Oid 23,
			valueData   = fromString (show n),
			valueFormat = P.Text
		}

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

	unpack _ =
		Nothing

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
			case n of { 15 -> "F"; 14 -> "E"; 13 -> "D"; 12 -> "C"; 11 -> "B";
			            10 -> "A"; 9  -> "9"; 8  -> "8"; 7  -> "7"; 6  -> "6";
			            5  -> "5"; 4  -> "4"; 3  -> "3"; 2  -> "2"; 1  -> "1";
			            _  -> "0" }

instance ColumnType B.ByteString where
	pack bs =
		Value {
			valueType   = P.Oid 17,
			valueData   = hex,
			valueFormat = P.Text
		}
		where
			hex = "\\x" <> B.concatMap word8ToHex bs

	unpack _ =
		Nothing

	columnTypeDescription =
		ColumnTypeDescription {
			columnTypeName = "bytea",
			columnTypeNull = False
		}

class Table a where
	insertStatement :: a -> Statement
	updateStatement :: Int64 -> a -> Statement
	createStatement :: Proxy a -> Statement
	dropStatement   :: Proxy a -> Statement
