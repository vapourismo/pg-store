{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}

module Database.PostgreSQL.Store.QuerySpec (querySpec) where

import           Test.Hspec
import           Data.String
import           Data.Typeable
import           Database.PostgreSQL.Store
import           Database.PostgreSQL.Store.Query
import           Database.PostgreSQL.Store.Columns

data MyType = MyConstructor {
	myRecord :: Int
} deriving (Show)

instance DescribableTable MyType where
	describeTableName _ = "InsertTableName"
	describeTableIdentifier _ = "InsertTableIdentifier"

testValue :: Int
testValue = 1337

querySpec :: Spec
querySpec =
	describe "pgsq" $ do
		it "table name" $
			queryStatement [pgsq|MyType|]
				`shouldBe` fromString ("\"" ++ describeTableName (Proxy :: Proxy MyType) ++ "\"")

		it "table identifier" $
			queryStatement [pgsq|&MyType|]
				`shouldBe` fromString ("\"" ++ describeTableIdentifier (Proxy :: Proxy MyType) ++ "\"")

		it "variable" $ do
			[pgsq|$testValue|] `shouldBe` Query "$1" [pack testValue]
