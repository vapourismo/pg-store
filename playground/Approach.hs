{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}

module Main (main) where

import           Data.List hiding (insert)
import           Data.Proxy
import           Data.String
import qualified Data.ByteString as B

import           Control.Monad
import           Control.Monad.Except

import qualified Database.PostgreSQL.LibPQ as P
import           Database.PostgreSQL.Store
import           Database.PostgreSQL.Store.Query (quoteIdentifier)

data BenchTable = BenchTable {
	field1 :: Int,
	field2 :: String,
	field3 :: Word
} deriving (Show, Eq, Ord)

mkTable ''BenchTable []

data TableInformation = TableInformation {
	tiName        :: String,
	tiIdentColumn :: String,
	tiColumns     :: [String]
} deriving (Show, Eq, Ord)

class InspectTable a where
	expandRow :: a -> [Value]

	tableInfo :: Proxy a -> TableInformation

instance InspectTable BenchTable where
	expandRow (BenchTable a b c) =
		[pack a, pack b, pack c]

	tableInfo proxy =
		TableInformation (tableName proxy) "$id" ["field1", "field2", "field3"]

-- |
insert2 :: (InspectTable a) => a -> Errand (Reference a)
insert2 row = do
	rs <- query (Query ({-# SCC queryString_eval #-} fromString queryString) (expandRow row))
	case rs of
		(ref : _) -> pure ref
		_         -> throwError EmptyResult

	where
		TableInformation name identCol cols =
			tableInfo ((const Proxy :: a -> Proxy a) row)

		queryString =
			{-# SCC queryString_build #-}
			"INSERT INTO " ++ quoteIdentifier name ++ " (" ++
			intercalate "," (map quoteIdentifier cols) ++
			") VALUES (" ++
			intercalate "," (map (\ idx -> "$" ++ show idx) [1 .. length cols]) ++
			") RETURNING " ++ quoteIdentifier identCol

-- |
main :: IO ()
main = do
	db <- P.connectdb "postgres://pgstore@localhost/pgstore"

	res <- runErrand db $ do
		query_ $(mkCreateQuery ''BenchTable)
		forM_ [1 .. 100000] $ \ i ->
			insert (BenchTable i "Hello World" (fromIntegral (i * 2)))

	print res
