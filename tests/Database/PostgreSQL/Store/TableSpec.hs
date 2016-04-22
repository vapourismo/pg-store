{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}

module Database.PostgreSQL.Store.TableSpec (tableSpec) where

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

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
	ttLazyByteString :: BL.ByteString,
	ttMaybeInt       :: Maybe Int
} deriving (Show, Eq, Ord)

instance Arbitrary TestTable where
	arbitrary =
		TestTable <$> arbitrary
		          <*> arbitrary
		          <*> arbitrary
		          <*> arbitrary
		          <*> arbitrary
		          <*> arbitrary
		          <*> arbitrary
		          <*> fmap T.pack arbitrary
		          <*> fmap TL.pack arbitrary
		          <*> fmap B.pack arbitrary
		          <*> fmap BL.pack arbitrary
		          <*> arbitrary

mkTable ''TestTable []

tableSpec :: P.Connection -> Spec
tableSpec con = do
	describe "Table" $ do
		it "create" $ do
			result <- runErrand con (query_ $(mkCreateQuery ''TestTable)) :: IO (Either ErrandError ())
			result `shouldBe` Right ()

	describe "Row" $ do
		it "insert/find/update/find/delete" $ monadicIO $ do
			row1 <- pick arbitrary :: PropertyM IO TestTable
			eRef <- run (runErrand con (insert row1))

			assert $
				case eRef of
					Right (Reference _) -> True
					_                   -> False

			let Right ref = eRef
			eRow1 <- run (runErrand con (find ref))

			assert $
				case eRow1 of
					Right (Row _ row1') -> row1' == row1
					_                   -> False

			row2 <- pick arbitrary :: PropertyM IO TestTable
			eUpdate <- run (runErrand con (update ref row2))

			assert (eUpdate == Right ())

			eRow2 <- run (runErrand con (find ref))

			assert $
				case eRow2 of
					Right (Row _ row2') -> row2' == row2
					_                   -> False

			eDelete <- run (runErrand con (delete ref))

			assert (eDelete == Right ())

	describe "Table" $ do
		it "drop" $ do
			result <- runErrand con (query_ [pgsq| DROP TABLE TestTable |]) :: IO (Either ErrandError ())
			result `shouldBe` Right ()
