module Database.PostgreSQL.Store.ColumnsSpec (columnsSpec) where

import           Data.Int
import           Data.Typeable
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import           Test.QuickCheck
import           Test.Hspec

import           Database.PostgreSQL.Store.Columns

testColumnHomomorphism :: (Column a, Show a, Eq a) => a -> Expectation
testColumnHomomorphism val =
	shouldBe (unpack (pack val)) (Just val)

propColumnHomomorphism :: (Column a, Arbitrary a, Eq a, Show a) => Proxy a -> Property
propColumnHomomorphism proxy =
	property (compare proxy)
	where
		compare :: (Column a, Arbitrary a, Eq a, Show a) => Proxy a -> a -> Bool
		compare _ x =
			unpack (pack x) == Just x

propColumnHomomorphism' :: (Arbitrary b, Show b, Column a, Eq a, Show a) => ([b] -> a) -> Property
propColumnHomomorphism' make =
	property (\ xs -> unpack (pack (make xs)) == Just (make xs))

columnsSpec :: Spec
columnsSpec = do
	describe "instance Column Bool" $
		it "homomorphic" $ do
			testColumnHomomorphism True
			testColumnHomomorphism False

	describe "instance Column Int" $ do
		it "homomorphic" $ do
			testColumnHomomorphism (minBound :: Int)
			testColumnHomomorphism (0 :: Int)
			testColumnHomomorphism (maxBound :: Int)

		it "homomorphic (qc)" $
			propColumnHomomorphism (Proxy :: Proxy Int)

	describe "instance Column Int8" $ do
		it "homomorphic" $ do
			testColumnHomomorphism (minBound :: Int8)
			testColumnHomomorphism (0 :: Int8)
			testColumnHomomorphism (maxBound :: Int8)

		it "homomorphic (qc)" $
			propColumnHomomorphism (Proxy :: Proxy Int8)

	describe "instance Column Int16" $ do
		it "homomorphic" $ do
			testColumnHomomorphism (minBound :: Int16)
			testColumnHomomorphism (0 :: Int16)
			testColumnHomomorphism (maxBound :: Int16)

		it "homomorphic (qc)" $
			propColumnHomomorphism (Proxy :: Proxy Int16)

	describe "instance Column Int32" $ do
		it "homomorphic" $ do
			testColumnHomomorphism (minBound :: Int32)
			testColumnHomomorphism (0 :: Int32)
			testColumnHomomorphism (maxBound :: Int32)

		it "homomorphic (qc)" $
			propColumnHomomorphism (Proxy :: Proxy Int32)

	describe "instance Column Int64" $ do
		it "homomorphic" $ do
			testColumnHomomorphism (minBound :: Int64)
			testColumnHomomorphism (0 :: Int64)
			testColumnHomomorphism (maxBound :: Int64)

		it "homomorphic (qc)" $
			propColumnHomomorphism (Proxy :: Proxy Int64)

	describe "instance Column [Char]" $ do
		it "homomorphic" $
			testColumnHomomorphism ([] :: [Char])

		it "homomorphic (qc)" $
			propColumnHomomorphism (Proxy :: Proxy [Char])

	describe "instance Column B.ByteString" $ do
		it "homomorphic" $
			testColumnHomomorphism B.empty

		it "homomorphic (qc)" $
			propColumnHomomorphism' B.pack

	describe "instance Column BL.ByteString" $ do
		it "homomorphic" $
			testColumnHomomorphism BL.empty

		it "homomorphic (qc)" $
			propColumnHomomorphism' BL.pack

	describe "instance Column T.Text" $ do
		it "homomorphic" $
			testColumnHomomorphism T.empty

		it "homomorphic (qc)" $
			propColumnHomomorphism' T.pack

	describe "instance Column TL.Text" $ do
		it "homomorphic" $
			testColumnHomomorphism TL.empty

		it "homomorphic (qc)" $
			propColumnHomomorphism' TL.pack
