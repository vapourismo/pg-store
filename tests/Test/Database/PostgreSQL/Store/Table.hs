{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}

module Test.Database.PostgreSQL.Store.Table (
	allTableSpecs
) where

import           Test.Hspec

import           Data.Proxy

import           Database.PostgreSQL.Store.Table
import           Database.PostgreSQL.Store.Query
import           Database.PostgreSQL.Store.Errand

import qualified Database.PostgreSQL.LibPQ as P

data OnlyEvens = OnlyEvens {
	value :: Int
} deriving (Show, Eq)

mkTable ''OnlyEvens
	[Unique ['value],
	 Check [pgss| value % 2 = 0 |]]

allTableSpecs :: Maybe P.Connection -> Spec
allTableSpecs mbCon = do
	maybe (pure ()) liveSpecs mbCon

dropTable :: P.Connection -> IO ()
dropTable con =
	() <$ runErrand con (query_ [pgsq| DROP TABLE @OnlyEvens |])

clearTable :: P.Connection -> IO ()
clearTable con =
	() <$ runErrand con (query_ [pgsq| DELETE FROM @OnlyEvens |])

liveSpecs :: P.Connection -> Spec
liveSpecs con =
	describe "Database.PostgreSQL.Store.Table (live)" $
		afterAll_ (dropTable con) $ describe "auto-generated instance Table" $
			after_ (clearTable con) $ do
				it "creates tables" $ do
					res1 <- runErrand con $
						query_ $(mkCreateQuery ''OnlyEvens)
					shouldBe res1 (Right ())

					-- Table exists now
					let table = tableName (Proxy :: Proxy OnlyEvens)
					res2 <- runErrand con $
						query [pgsq| SELECT COUNT(*) FROM pg_tables WHERE tablename = $table |]
					shouldBe res2 (Right [Single (1 :: Int)])

				it "inserts a rows" $ do
					res1 <- runErrand con (insert (OnlyEvens 2))
					shouldSatisfy res1 $ \ r ->
						case r of
							Right (Reference _) -> True
							_                   -> False

					-- Has been inserted correctly
					res2 <- runErrand con $
						query [pgsq| SELECT #OnlyEvens FROM @OnlyEvens |]
					shouldBe res2 (Right [OnlyEvens 2])

				it "respects constraints while inserting" $ do
					-- Violate UNIQUE
					res3 <- runErrand con $ do
						insert (OnlyEvens 2)
						insert (OnlyEvens 2)
					shouldSatisfy res3 $ \ r ->
						case r of
							Left (UniqueViolation _ _) -> True
							_                          -> False

					-- Violate CHECK
					res4 <- runErrand con (insert (OnlyEvens 1))
					shouldSatisfy res4 $ \ r ->
						case r of
							Left (CheckViolation _ _) -> True
							_                         -> False

				it "inserts many rows" $ do
					res1 <- runErrand con (insertMany (map OnlyEvens [2, 4 .. 100]))
					shouldSatisfy res1 $ \ r ->
						case r of
							Right xs -> length xs == 50
							_        -> False

					-- Verify that there are actually 50 present
					res2 <- runErrand con $
						query [pgsq| SELECT COUNT(*) FROM @OnlyEvens |]
					shouldBe res2 (Right [Single (50 :: Int)])

				it "finds a row" $ do
					res1 <- runErrand con $ do
						ref <- insert (OnlyEvens 4)
						find ref
					shouldBe res1 (Right (OnlyEvens 4))

				it "finds many rows" $
					pendingWith "Not yet implemented"

				it "updates a row" $ do
					res1 <- runErrand con $ do
						ref <- insert (OnlyEvens 4)
						update ref (OnlyEvens 6)
						find ref
					shouldBe res1 (Right (OnlyEvens 6))

				it "respects constraints while updating" $ do
					-- Violate UNIQUE
					res1 <- runErrand con $ do
						insert (OnlyEvens 2)
						ref <- insert (OnlyEvens 4)
						update ref (OnlyEvens 2)
					shouldSatisfy res1 $ \ r ->
						case r of
							Left (UniqueViolation _ _) -> True
							_                          -> False

					-- Vioate CHECK
					res2 <- runErrand con $ do
						ref <- insert (OnlyEvens 6)
						update ref (OnlyEvens 7)
					shouldSatisfy res2 $ \ r ->
						case r of
							Left (CheckViolation _ _) -> True
							_                         -> False

				it "updates many rows" $
					pendingWith "Not yet implemented"

				it "deletes a row" $ do
					res1 <- runErrand con $ do
						ref <- insert (OnlyEvens 2)
						delete ref
					shouldBe res1 (Right ())

				it "deletes many rows" $
					pendingWith "Not yet implemented"
