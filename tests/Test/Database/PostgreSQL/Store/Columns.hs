{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}

module Test.Database.PostgreSQL.Store.Columns (
	allColumnsSpecs
) where

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

import           Control.Monad.Trans

import           Data.Int
import           Data.Word
import           Data.Proxy

import           Database.PostgreSQL.Store.Query
import           Database.PostgreSQL.Store.Columns
import           Database.PostgreSQL.Store.Result
import           Database.PostgreSQL.Store.Errand

import qualified Database.PostgreSQL.LibPQ as P

testLivePackUnpack :: (Column a, Arbitrary a, Eq a, Show a) => Proxy a -> P.Connection -> Property
testLivePackUnpack proxy con =
	property $ coerceParam proxy $ \ value -> monadicIO $ do
		lift $ do
			res <- runErrand con (queryWith (Query "SELECT $1" [pack value]) unpackColumn)

			shouldBe res (Right [value])
	where
		coerceParam :: Proxy a -> (a -> b) -> a -> b
		coerceParam _ f x = f x

testPackUnpack :: (Column a, Arbitrary a, Eq a, Show a) => Proxy a -> Property
testPackUnpack proxy =
	property (compare proxy)
	where
		compare :: (Column a, Eq a) => Proxy a -> a -> Bool
		compare _ x = unpack (pack x) == Just x

allColumnsSpecs :: Maybe P.Connection -> Spec
allColumnsSpecs mbCon = do
	describe "Database.PostgreSQL.Store.Columns" $ do
		describe "instance (Column a) => Column (Maybe a)" $
			it "behaves isomorphic" (testPackUnpack (Proxy :: Proxy (Maybe Bool)))

		describe "instance Column Bool" $
			it "behaves isomorphic" (testPackUnpack (Proxy :: Proxy Bool))

		describe "instance Column Int" $
			it "behaves isomorphic" (testPackUnpack (Proxy :: Proxy Int))

		describe "instance Column Int8" $
			it "behaves isomorphic" (testPackUnpack (Proxy :: Proxy Int8))

		describe "instance Column Int16" $
			it "behaves isomorphic" (testPackUnpack (Proxy :: Proxy Int16))

		describe "instance Column Int32" $
			it "behaves isomorphic" (testPackUnpack (Proxy :: Proxy Int32))

		describe "instance Column Int64" $
			it "behaves isomorphic" (testPackUnpack (Proxy :: Proxy Int64))

		describe "instance Column Word" $
			it "behaves isomorphic" (testPackUnpack (Proxy :: Proxy Word))

		describe "instance Column Word8" $
			it "behaves isomorphic" (testPackUnpack (Proxy :: Proxy Word8))

		describe "instance Column Word16" $
			it "behaves isomorphic" (testPackUnpack (Proxy :: Proxy Word16))

		describe "instance Column Word32" $
			it "behaves isomorphic" (testPackUnpack (Proxy :: Proxy Word32))

		describe "instance Column Word64" $
			it "behaves isomorphic" (testPackUnpack (Proxy :: Proxy Word64))

	maybe (pure ()) liveSpecs mbCon

liveSpecs :: P.Connection -> Spec
liveSpecs con =
	describe "Database.PostgreSQL.Store.Columns (live)" $ do
		describe "instance Column Bool" $
			it "behaves isomorphic" (testLivePackUnpack (Proxy :: Proxy Bool) con)

		describe "instance Column Int" $
			it "behaves isomorphic" (testLivePackUnpack (Proxy :: Proxy Int) con)

		describe "instance Column Int8" $
			it "behaves isomorphic" (testLivePackUnpack (Proxy :: Proxy Int8) con)

		describe "instance Column Int16" $
			it "behaves isomorphic" (testLivePackUnpack (Proxy :: Proxy Int16) con)

		describe "instance Column Int32" $
			it "behaves isomorphic" (testLivePackUnpack (Proxy :: Proxy Int32) con)

		describe "instance Column Int64" $
			it "behaves isomorphic" (testLivePackUnpack (Proxy :: Proxy Int64) con)

		describe "instance Column Word" $
			it "behaves isomorphic" (testLivePackUnpack (Proxy :: Proxy Word) con)

		describe "instance Column Word8" $
			it "behaves isomorphic" (testLivePackUnpack (Proxy :: Proxy Word8) con)

		describe "instance Column Word16" $
			it "behaves isomorphic" (testLivePackUnpack (Proxy :: Proxy Word16) con)

		describe "instance Column Word32" $
			it "behaves isomorphic" (testLivePackUnpack (Proxy :: Proxy Word32) con)

		describe "instance Column Word64" $
			it "behaves isomorphic" (testLivePackUnpack (Proxy :: Proxy Word64) con)
