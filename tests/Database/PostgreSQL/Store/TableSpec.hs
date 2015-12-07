{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

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

tableSpec :: P.Connection -> Spec
tableSpec con =
	describe "Table" $ do
		it "created" $ do
			result <- runErrand con (query $(mkCreateQuery ''TestTable)) :: IO (Either ErrandError [()])
			result `shouldBe` Right []

		it "dropped" $ do
			result <- runErrand con (query [pgsq| DROP TABLE TestTable |]) :: IO (Either ErrandError [()])
			result `shouldBe` Right []
