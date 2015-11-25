{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Database.PostgreSQL.Store.Types where

import           Data.Int
import           Data.Word
import           Data.Bits
import           Data.Monoid
import           Data.String
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

class ColumnType a where
	pack   :: a -> Value
	unpack :: Value -> Maybe a

instance ColumnType Int where
	pack n =
		Value {
			valueType   = P.Oid 23,
			valueData   = fromString (show n),
			valueFormat = P.Text
		}

	unpack _ =
		Nothing

instance ColumnType Int64 where
	pack n =
		Value {
			valueType   = P.Oid 20,
			valueData   = fromString (show n),
			valueFormat = P.Text
		}

	unpack _ =
		Nothing

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

class Table a where
	insert :: a -> Statement
	update :: Int64 -> a -> Statement
