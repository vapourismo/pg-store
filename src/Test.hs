{-# LANGUAGE OverloadedStrings, TemplateHaskell, RecordWildCards, QuasiQuotes, FlexibleInstances #-}

module Test where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Except

import qualified Data.ByteString as B

import           Database.PostgreSQL.Store
import           Database.PostgreSQL.Store.Types
import           Database.PostgreSQL.Store.Monad
import qualified Database.PostgreSQL.LibPQ as P

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

data XError
	= NoResult
	| ResultError ResultError
	deriving Show

type X = ReaderT P.Connection (ExceptT XError IO)

runX :: P.Connection -> X a -> ExceptT XError IO a
runX = flip runReaderT

query_ :: Query -> X ()
query_ Query {..} = do
	con <- ask
	lift (ExceptT (maybe (Left NoResult) (const (pure ())) <$> P.execParams con queryStatement (map makeParam queryParams) P.Text))
	where
		makeParam Value {..} =
			Just (valueType, valueData, valueFormat)

query :: (ResultRow a) => Query -> X [a]
query Query {..} = do
	con <- ask
	lift $ do
		result <- ExceptT (maybe (Left NoResult) pure <$> P.execParams con queryStatement (map makeParam queryParams) P.Text)
		withExceptT ResultError (processResult result fromResult)
	where
		makeParam Value {..} =
			Just (valueType, valueData, valueFormat)

run :: (Show a) => P.Connection -> X a -> IO ()
run con x =
	runExceptT (runX con x) >>= print

test :: IO ()
test = do
	con <- P.connectdb "postgres://localhost/ole"
	P.status con >>= print

	run con $ do
		query_ $(mkCreateQuery ''Movie)

	run con $ do
		query_ (insertQuery (Movie "Test Movie 1" 2001))
		query_ (insertQuery (Movie "Test Movie 2" 2002))
		query_ (insertQuery (Movie "Test Movie 3" 2003))
		query_ (insertQuery (Movie "Test Movie 4" 2004))

	run con $ do
		let name = "Test Movie%" :: B.ByteString
		let q = [pgsq| SELECT * FROM Movie WHERE title LIKE $name AND year > 2002 |]
		query q :: X [Row Movie]

	run con $ do
		query_ $(mkDropQuery ''Movie)

	P.finish con
