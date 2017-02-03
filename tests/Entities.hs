{-# LANGUAGE OverloadedStrings, QuasiQuotes, TypeApplications #-}

module Main (main) where

import           Control.Monad.Trans

import           Data.Int
import           Data.Word
import           Numeric.Natural
import           Data.Scientific

import           Data.Maybe
import           Data.String

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as TL

import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Test.Framework
import           Test.Framework.Providers.QuickCheck2

import qualified Database.PostgreSQL.LibPQ as P

import           Database.PostgreSQL.Store.Entity
import           Database.PostgreSQL.Store.Errand
import           Database.PostgreSQL.Store.Query

import           System.Environment

instance Arbitrary B.ByteString where
	arbitrary = B.pack <$> arbitrary

	shrink bs
		| B.null bs = [bs]
		| otherwise = B.tail bs : shrink (B.tail bs)

instance Arbitrary BL.ByteString where
	arbitrary = BL.pack <$> arbitrary

	shrink bs
		| BL.null bs = [bs]
		| otherwise = BL.tail bs : shrink (BL.tail bs)

instance Arbitrary T.Text where
	arbitrary = T.pack <$> arbitrary

	shrink bs
		| T.null bs = [bs]
		| otherwise = T.tail bs : shrink (T.tail bs)

instance Arbitrary TL.Text where
	arbitrary = TL.pack <$> arbitrary

	shrink bs
		| TL.null bs = [bs]
		| otherwise = TL.tail bs : shrink (TL.tail bs)

instance Arbitrary Scientific where
	arbitrary = scientific <$> arbitrary <*> arbitrary

-- |
testEntity :: (Entity a, Eq a, Show a) => P.Connection -> a -> Property
testEntity db x = monadicIO $ do
	result <- lift (runErrand db (query [pgQuery| SELECT $x |]))
	elem <- case result of
		Left err  -> fail ("ErrandError: " ++ show err)
		Right [x] -> pure x
		Right xs  -> fail ("Weird result: " ++ show xs)

	stop (elem === x)

-- |
testEntityWith :: (Entity a, Eq a, Show a) => P.Connection -> (a -> a) -> a -> Property
testEntityWith db f x =
	testEntity db (f x)

-- |
filterStringNulls :: String -> String
filterStringNulls = filter (/= '\NUL')

-- |
filterTextNulls :: T.Text -> T.Text
filterTextNulls x = T.concat (T.split (== '\NUL') x)

-- |
filterLazyTextNulls :: TL.Text -> TL.Text
filterLazyTextNulls x = TL.concat (TL.split (== '\NUL') x)

-- | Test entry point
main :: IO ()
main = do
	pgInfo <- lookupEnv "PGINFO"
	db <- P.connectdb (fromString (fromMaybe "user=pgstore dbname=pgstore" pgInfo))

	defaultMain
		[
			testProperty "entity-bool"            (testEntity @Bool db),

			testProperty "entity-integer"         (testEntity @Integer db),
			testProperty "entity-int"             (testEntity @Int db),
			testProperty "entity-int8"            (testEntity @Int8 db),
			testProperty "entity-int16"           (testEntity @Int16 db),
			testProperty "entity-int32"           (testEntity @Int32 db),
			testProperty "entity-int64"           (testEntity @Int64 db),

			testProperty "entity-natural"         (testEntity @Natural db),
			testProperty "entity-word"            (testEntity @Word db),
			testProperty "entity-word8"           (testEntity @Word8 db),
			testProperty "entity-word16"          (testEntity @Word16 db),
			testProperty "entity-word32"          (testEntity @Word32 db),
			testProperty "entity-word64"          (testEntity @Word64 db),

			testProperty "entity-float"           (testEntity @Float db),
			testProperty "entity-double"          (testEntity @Double db),

			testProperty "entity-scientific"      (testEntity @Scientific db),

			testProperty "entity-bytestring"      (testEntity @B.ByteString db),
			testProperty "entity-bytestring-lazy" (testEntity @BL.ByteString db),
			testProperty "entity-string"          (testEntityWith @String db filterStringNulls),
			testProperty "entity-text"            (testEntityWith @T.Text db filterTextNulls),
			testProperty "entity-text-lazy"       (testEntityWith @TL.Text db filterLazyTextNulls)
		]
