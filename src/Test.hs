{-# LANGUAGE OverloadedStrings, TemplateHaskell, RecordWildCards, QuasiQuotes, FlexibleInstances #-}

module Test where

import qualified Data.ByteString as B

import           Database.PostgreSQL.Store
import           Database.PostgreSQL.Store.Internal
import qualified Database.PostgreSQL.LibPQ as P

data Movie = Movie {
	title  :: B.ByteString,
	year   :: Int
} deriving Show

mkTable ''Movie

data Actor = Actor {
	movie     :: Reference Movie,
	firstName :: B.ByteString,
	lastName  :: B.ByteString
} deriving Show

mkTable ''Actor

test :: IO ()
test = do
	con <- P.connectdb "postgres://localhost/ole"
	P.status con >>= print

	runErrand con $ do
		query_ $(mkCreateQuery ''Movie)

	runErrand con $ do
		query_ (insertQuery (Movie "Test Movie 1" 2001))
		query_ (insertQuery (Movie "Test Movie 2" 2002))
		query_ (insertQuery (Movie "Test Movie 3" 2003))
		query_ (insertQuery (Movie "Test Movie 4" 2004))

	runErrand con $ do
		let q = [pgsq| SELECT * FROM Movie WHERE &Movie IN (1, 3) |]
		query q :: Errand [Row Movie]

	runErrand con $ do
		query_ [pgsq| DROP TABLE Movie |]

	P.finish con
