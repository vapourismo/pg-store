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

-- | Pack and unpack value to test if the input value is the same as the output value.
testColumnIsomorphism :: (Column a, Show a, Eq a) => a -> Expectation
testColumnIsomorphism val =
	shouldBe (unpack (pack val)) (Just val)

-- | Quick check an instance of Column to see if it behaves isomorphic.
propColumnIsomorphism :: (Column a, Arbitrary a, Eq a, Show a) => Proxy a -> Property
propColumnIsomorphism proxy =
	property (compare proxy)
	where
		compare :: (Column a, Arbitrary a, Eq a, Show a) => Proxy a -> a -> Bool
		compare _ x =
			unpack (pack x) == Just x

-- | Quick check an instance of Column which has to be generated first, because it does not have an
--   instance of Arbitrary.
propColumnIsomorphism' :: (Arbitrary b, Show b, Column a, Eq a, Show a) => ([b] -> a) -> Property
propColumnIsomorphism' make =
	property (\ xs -> unpack (pack (make xs)) == Just (make xs))

-- | Test for instances of Column
columnsSpec :: Spec
columnsSpec = do
	describe "instance Column Bool" $
		it "must behave isomorphic" $ do
			testColumnIsomorphism True
			testColumnIsomorphism False

	describe "instance Column Int" $ do
		it "must behave isomorphic" $ do
			testColumnIsomorphism (minBound :: Int)
			testColumnIsomorphism (0 :: Int)
			testColumnIsomorphism (maxBound :: Int)

		it "must behave isomorphic (using QuickCheck)" $
			propColumnIsomorphism (Proxy :: Proxy Int)

	describe "instance Column Int8" $ do
		it "must behave isomorphic" $ do
			testColumnIsomorphism (minBound :: Int8)
			testColumnIsomorphism (0 :: Int8)
			testColumnIsomorphism (maxBound :: Int8)

		it "must behave isomorphic (using QuickCheck)" $
			propColumnIsomorphism (Proxy :: Proxy Int8)

	describe "instance Column Int16" $ do
		it "must behave isomorphic" $ do
			testColumnIsomorphism (minBound :: Int16)
			testColumnIsomorphism (0 :: Int16)
			testColumnIsomorphism (maxBound :: Int16)

		it "must behave isomorphic (using QuickCheck)" $
			propColumnIsomorphism (Proxy :: Proxy Int16)

	describe "instance Column Int32" $ do
		it "must behave isomorphic" $ do
			testColumnIsomorphism (minBound :: Int32)
			testColumnIsomorphism (0 :: Int32)
			testColumnIsomorphism (maxBound :: Int32)

		it "must behave isomorphic (using QuickCheck)" $
			propColumnIsomorphism (Proxy :: Proxy Int32)

	describe "instance Column Int64" $ do
		it "must behave isomorphic" $ do
			testColumnIsomorphism (minBound :: Int64)
			testColumnIsomorphism (0 :: Int64)
			testColumnIsomorphism (maxBound :: Int64)

		it "must behave isomorphic (using QuickCheck)" $
			propColumnIsomorphism (Proxy :: Proxy Int64)

	describe "instance Column [Char]" $ do
		it "must behave isomorphic" $
			testColumnIsomorphism ([] :: [Char])

		it "must behave isomorphic (using QuickCheck)" $
			propColumnIsomorphism (Proxy :: Proxy [Char])

	describe "instance Column B.ByteString" $ do
		it "must behave isomorphic" $
			testColumnIsomorphism B.empty

		it "must behave isomorphic (using QuickCheck)" $
			propColumnIsomorphism' B.pack

	describe "instance Column BL.ByteString" $ do
		it "must behave isomorphic" $
			testColumnIsomorphism BL.empty

		it "must behave isomorphic (using QuickCheck)" $
			propColumnIsomorphism' BL.pack

	describe "instance Column T.Text" $ do
		it "must behave isomorphic" $
			testColumnIsomorphism T.empty

		it "must behave isomorphic (using QuickCheck)" $
			propColumnIsomorphism' T.pack

	describe "instance Column TL.Text" $ do
		it "must behave isomorphic" $
			testColumnIsomorphism TL.empty

		it "must behave isomorphic (using QuickCheck)" $
			propColumnIsomorphism' TL.pack
