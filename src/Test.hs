{-# LANGUAGE OverloadedStrings, TemplateHaskell, RecordWildCards, QuasiQuotes, FlexibleInstances #-}

module Test where

import qualified Data.ByteString as B

import           Database.PostgreSQL.Store
import           Database.PostgreSQL.Store.Internal
import qualified Database.PostgreSQL.LibPQ as P

data Movie = Movie {
	movieTitle  :: B.ByteString,
	movieYear   :: Int
} deriving Show

mkTable ''Movie

data Actor = Actor {
	actorName :: B.ByteString
} deriving Show

mkTable ''Actor

data PlayedIn = PlayedIn {
	playedInActor :: Reference Actor,
	playedInMovie :: Reference Movie
} deriving Show

mkTable ''PlayedIn

test :: IO ()
test = do
	con <- P.connectdb "postgres://localhost/ole"
	P.status con >>= print

	runErrand con $ do
		query_ $(mkCreateQuery ''Movie)
		query_ $(mkCreateQuery ''Actor)
		query_ $(mkCreateQuery ''PlayedIn)

	r <- runErrand con $ do
		mov1 <- insert (Movie "Test Movie 1" 2001)
		mov2 <- insert (Movie "Test Movie 2" 2002)
		mov3 <- insert (Movie "Test Movie 3" 2003)
		mov4 <- insert (Movie "Test Movie 4" 2004)

		act1 <- insert (Actor "Actor 1")
		act2 <- insert (Actor "Actor 2")
		act3 <- insert (Actor "Actor 3")

		insert (PlayedIn act1 mov1)
		insert (PlayedIn act1 mov2)
		insert (PlayedIn act1 mov3)

		insert (PlayedIn act2 mov2)
		insert (PlayedIn act2 mov3)
		insert (PlayedIn act2 mov4)

		insert (PlayedIn act3 mov3)

		query [pgsq| SELECT *
		             FROM Movie, PlayedIn
		             WHERE playedInMovie = &Movie AND
		                   playedInActor IN ($act2, $act3)
		             GROUP BY &PlayedIn, &Movie |] :: Errand [Row Movie]

	either print (mapM_ print) r

	runErrand con $ do
		query_ [pgsq| DROP TABLE IF EXISTS PlayedIn |]
		query_ [pgsq| DROP TABLE IF EXISTS Movie |]
		query_ [pgsq| DROP TABLE IF EXISTS Actor |]

	P.finish con
