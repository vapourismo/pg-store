{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}

module Database.PostgreSQL.Store.TableSpec (tableSpec) where

import           Test.Hspec

import           Data.Int
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import           Database.PostgreSQL.Store
import qualified Database.PostgreSQL.LibPQ as P

data TestTable = TestTable {
	ttBool           :: Bool,
	ttInt            :: Int,
	ttInt8           :: Int8,
	ttInt16          :: Int16,
	ttInt32          :: Int32,
	ttInt64          :: Int64,
	ttString         :: String,
	ttText           :: T.Text,
	ttLazyText       :: TL.Text,
	ttByteString     :: B.ByteString,
	ttLazyByteString :: BL.ByteString
} deriving (Show, Eq, Ord)

mkTable ''TestTable

testTableRow1 :: TestTable
testTableRow1 =
	TestTable
		True
		13 37 1337 7331 42
		"Hello World"
		"Hello World" "Hello Lazy"
		"Hello World" "Hello Lazy"

testTableRow2 :: TestTable
testTableRow2 =
	TestTable
		False
		42 73 7331 1337 13
		"World"
		"Hello" "Lazy"
		"Hello" "Lazy"

tableSpec :: P.Connection -> Spec
tableSpec con =
	describe "Table" $ do
		it "created" $ do
			result <- runErrand con (query_ $(mkCreateQuery ''TestTable)) :: IO (Either ErrandError ())
			result `shouldBe` Right ()

		it "interact" $ do
			-- Create
			eRef <- runErrand con (insert testTableRow1)
			eRef `shouldSatisfy` \ r ->
				case r of
					Right (Reference _) -> True
					_                   -> False

			let Right ref = eRef

			-- Find
			eRes <- runErrand con (find ref)
			eRes `shouldSatisfy` \ r ->
				case r of
					Right (Row _ _) -> True
					_               -> False

			let Right (Row _ row1) = eRes

			row1 `shouldBe` testTableRow1

			-- Update
			eRes2 <- runErrand con (update ref testTableRow2)
			eRes2 `shouldBe` Right ()

			-- Find
			eRes3 <- runErrand con (find ref)
			eRes3 `shouldSatisfy` \ r ->
				case r of
					Right (Row _ _) -> True
					_               -> False

			let Right (Row _ row2) = eRes3

			row2 `shouldBe` testTableRow2

			-- Delete
			eRes4 <- runErrand con (delete ref)
			eRes4 `shouldBe` Right ()

		it "dropped" $ do
			result <- runErrand con (query_ [pgsq| DROP TABLE TestTable |]) :: IO (Either ErrandError ())
			result `shouldBe` Right ()
