{-# LANGUAGE OverloadedStrings, TemplateHaskell, RecordWildCards, ExistentialQuantification,
             StandaloneDeriving #-}

module Database.PostgreSQL.Store.Internal where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Except

import           Data.Int
import           Data.Word
import           Data.Bits
import           Data.List
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

-- | Process the result.
processResult :: P.Result -> ResultProcessor a -> ExceptT ResultError IO a
processResult = flip runReaderT

-- | Get the column number for a column name.
columnNumber :: B.ByteString -> ResultProcessor P.Column
columnNumber name = do
	result <- ask
	lift (ExceptT (maybe (Left (UnknownColumnName name)) pure <$> P.fnumber result name))

-- | Get the type of a column.
columnType :: P.Column -> ResultProcessor P.Oid
columnType col = do
	result <- ask
	lift (lift (P.ftype result col))

-- | Get the format of a column.
columnFormat :: P.Column -> ResultProcessor P.Format
columnFormat col = do
	result <- ask
	lift (lift (P.fformat result col))

-- | Get information about a column.
columnInfo :: B.ByteString -> ResultProcessor (P.Column, P.Oid, P.Format)
columnInfo name = do
	col <- columnNumber name
	(,,) col <$> columnType col <*> columnFormat col

-- | Do something for each row.
foreachRow :: (P.Row -> ResultProcessor a) -> ResultProcessor [a]
foreachRow rowProcessor = do
	result <- ask
	numRows <- lift (lift (P.ntuples result))
	mapM rowProcessor [0 .. numRows - 1]

-- | Get cell data.
cellData :: P.Row -> P.Column -> ResultProcessor B.ByteString
cellData row col = do
	result <- ask
	lift (ExceptT (maybe (Left (ColumnDataMissing row col)) pure <$> P.getvalue' result row col))

-- | Get cell value.
cellValue :: P.Row -> (P.Column, P.Oid, P.Format) -> ResultProcessor Value
cellValue row (col, oid, fmt) = do
	Value oid <$> cellData row col <*> pure fmt

-- | Unpack cell value.
unpackCellValue :: (Column a) => P.Row -> (P.Column, P.Oid, P.Format) -> ResultProcessor a
unpackCellValue row info = do
	value <- cellValue row info
	lift (ExceptT (make columnDescription (unpack value)))
	where
		(col, oid, fmt) = info

		valueError desc =
			pure (Left (ValueError row col oid fmt desc))

		make :: ColumnDescription a -> Maybe a -> IO (Either ResultError a)
		make desc =
			maybe (valueError desc) (pure . pure)

-- | Description of a table type
data TableDescription a = TableDescription {
	-- | Table name
	tableName :: String,

	-- | Identifer column name
	tableIdentColumn :: String
} deriving (Show, Eq, Ord)

-- | Resolved row
data Row a = Row {
	-- | Identifier
	rowID :: Int64,

	-- | Value
	rowValue :: a
} deriving (Show, Eq, Ord)

-- | Reference to a row
newtype Reference a = Reference Int64
	deriving (Show, Eq, Ord)

-- | Error during errand
data ErrandError
	= NoResult
	| ExecError P.ExecStatus (Maybe B.ByteString)
	| ResultError ResultError
	| UserError String
	deriving Show

-- | Database errand
type Errand = ReaderT P.Connection (ExceptT ErrandError IO)

-- | Run an errand.
runErrand :: P.Connection -> Errand a -> IO (Either ErrandError a)
runErrand con errand =
	runExceptT (runReaderT errand con)

-- | Raise a user error.
raiseErrandError :: String -> Errand a
raiseErrandError msg =
	lift (ExceptT (pure (Left (UserError msg))))

-- | Execute a query and return its result.
executeQuery :: Query -> Errand P.Result
executeQuery (Query statement params) = do
	con <- ask
	lift $ do
		res <- ExceptT (transformResult <$> P.execParams con statement transformedParams P.Text)
		status <- lift (P.resultStatus res)

		case status of
			P.CommandOk -> pure res
			P.TuplesOk  -> pure res

			other -> do
				msg <- lift (P.resultErrorMessage res)
				throwE (ExecError other msg)

	where
		transformResult =
			maybe (Left NoResult) pure

		transformParam Value {..} = Just (valueType, valueData, valueFormat)
		transformParam NullValue  = Nothing

		transformedParams =
			map transformParam params

-- | Execute a query and process its result set.
query :: (ResultRow a) => Query -> Errand [a]
query qry = do
	result <- executeQuery qry
	lift (withExceptT ResultError (processResult result resultProcessor))

-- | Execute a query.
query_ :: Query -> Errand ()
query_ qry =
	() <$ executeQuery qry

-- | Table type
class Table a where
	-- | Insert a row into a table.
	insert :: a -> Errand (Row a)

	-- | Generate an UPDATE query which updates an existing row.
	update :: Row a -> Errand ()

	-- | Generate a DELETE query which removes an existing row.
	delete :: (HasID i) => i a -> Errand ()

	-- | Generate a CREATE query which creates the table.
	createQuery :: Proxy a -> Query

	-- | Extract rows of this table from the given result.
	tableResultProcessor :: ResultProcessor [Row a]

	-- | Extract references to rows of this table from a given result.
	tableRefResultProcessor :: ResultProcessor [Reference a]

	-- | Describe the table.
	tableDescription :: TableDescription a

-- | Result row
class ResultRow a where
	-- | Extract rows from the given result.
	resultProcessor :: ResultProcessor [a]

instance (ResultRow a, ResultRow b) => ResultRow (a, b) where
	resultProcessor =
		zip <$> resultProcessor
		    <*> resultProcessor

instance (ResultRow a, ResultRow b, ResultRow c) => ResultRow (a, b, c) where
	resultProcessor =
		zip3 <$> resultProcessor
		     <*> resultProcessor
		     <*> resultProcessor

instance (ResultRow a, ResultRow b, ResultRow c, ResultRow d) => ResultRow (a, b, c, d) where
	resultProcessor =
		zip4 <$> resultProcessor
		     <*> resultProcessor
		     <*> resultProcessor
		     <*> resultProcessor

instance (ResultRow a, ResultRow b, ResultRow c, ResultRow d, ResultRow e) => ResultRow (a, b, c, d, e) where
	resultProcessor =
		zip5 <$> resultProcessor
		     <*> resultProcessor
		     <*> resultProcessor
		     <*> resultProcessor
		     <*> resultProcessor

instance (ResultRow a, ResultRow b, ResultRow c, ResultRow d, ResultRow e, ResultRow f) => ResultRow (a, b, c, d, e, f) where
	resultProcessor =
		zip6 <$> resultProcessor
		     <*> resultProcessor
		     <*> resultProcessor
		     <*> resultProcessor
		     <*> resultProcessor
		     <*> resultProcessor

instance (ResultRow a, ResultRow b, ResultRow c, ResultRow d, ResultRow e, ResultRow f, ResultRow g) => ResultRow (a, b, c, d, e, f, g) where
	resultProcessor =
		zip7 <$> resultProcessor
		     <*> resultProcessor
		     <*> resultProcessor
		     <*> resultProcessor
		     <*> resultProcessor
		     <*> resultProcessor
		     <*> resultProcessor

instance (Table a) => ResultRow (Row a) where
	resultProcessor = tableResultProcessor

instance (Table a) => ResultRow (Reference a) where
	resultProcessor = tableRefResultProcessor

instance ResultRow () where
	resultProcessor = foreachRow (const (pure ()))

-- | A value of that type contains an ID.
class HasID a where
	-- | Retrieve the underlying ID.
	referenceID :: a b -> Int64

instance HasID Row where
	referenceID (Row rid _) = rid

instance HasID Reference where
	referenceID (Reference rid) = rid

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
