{-# LANGUAGE OverloadedStrings, TemplateHaskell, RecordWildCards, QuasiQuotes, FlexibleInstances #-}

module Test where

import           Control.Monad
import           Control.Monad.Trans.Maybe
import qualified Data.ByteString           as B
import qualified Database.PostgreSQL.LibPQ as P
import           Database.PostgreSQL.Store.TH
import           Database.PostgreSQL.Store.Types

data Movie = Movie {
	title  :: B.ByteString,
	year   :: Int
} deriving Show

data Actor = Actor {
	movie     :: Reference Movie,
	firstName :: B.ByteString,
	lastName  :: B.ByteString
} deriving Show

mkTable ''Movie
mkTable ''Actor

class QueryResult a where
	fromQueryResult :: P.Result -> MaybeT IO [a]

instance (Table a) => QueryResult (Reference a) where
	fromQueryResult = fromResult

executeQuery_ :: P.Connection -> Query -> MaybeT IO ()
executeQuery_ con Query {..} =
	() <$ MaybeT (P.execParams con queryStatement (map makeParam queryParams) P.Text)
	where
		makeParam Value {..} =
			Just (valueType, valueData, valueFormat)

executeQuery :: (QueryResult a) => P.Connection -> Query -> MaybeT IO [a]
executeQuery con Query {..} =
	fromQueryResult =<< MaybeT (P.execParams con queryStatement (map makeParam queryParams) P.Text)
	where
		makeParam Value {..} =
			Just (valueType, valueData, valueFormat)

test :: IO ()
test = do
	con <- P.connectdb "postgres://localhost/ole"
	P.status con >>= print

	runMaybeT $
		executeQuery_ con $(mkCreateQuery ''Movie)

	runMaybeT $ do
		executeQuery_ con (insertQuery (Movie "Test Movie 1" 2001))
		executeQuery_ con (insertQuery (Movie "Test Movie 2" 2002))
		executeQuery_ con (insertQuery (Movie "Test Movie 3" 2003))
		executeQuery_ con (insertQuery (Movie "Test Movie 4" 2004))

	let name = "Test Movie 1" :: B.ByteString
	let q = [pgsq| SELECT * FROM Movie WHERE title = $name |]
	print q
	Just result <- runMaybeT (executeQuery con q) :: IO (Maybe [Reference Movie])
	print result

	runMaybeT $
		executeQuery_ con $(mkDropQuery ''Movie)

	P.finish con
