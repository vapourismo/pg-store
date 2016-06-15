{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}

module Database.PostgreSQL.Store.QuerySpec (querySpec) where

import Test.Hspec

import Data.String
import Data.Typeable

import Database.PostgreSQL.Store
import Database.PostgreSQL.Store.Query
import Database.PostgreSQL.Store.Columns

data MyType = MyConstructor {
	myField :: Int
} deriving (Show)

instance DescribableTable MyType where
	describeTableName _ = "InsertTableName"
	describeTableIdentifier _ = "InsertTableIdentifier"

testValue :: Int
testValue = 1337

querySpec :: Spec
querySpec =
	describe "parseStoreQueryE" $ do
		it "must resolve tables names correctly" $
			queryStatement [pgsq|MyType|]
				`shouldBe` fromString ("\"" ++ describeTableName (Proxy :: Proxy MyType) ++ "\"")

		it "must resolve identifier column names correctly" $
			queryStatement [pgsq|&MyType|]
				`shouldBe` fromString ("\"" ++ describeTableIdentifier (Proxy :: Proxy MyType) ++ "\"")

		it "must insert and pack variables correctly" $
			[pgsq|$testValue|] `shouldBe` Query "$1" [pack testValue]

		it "must resolve fields correctly" $
			queryStatement [pgsq|myField|]
				`shouldBe` fromString ("\"Database.PostgreSQL.Store.QuerySpec.myField\"")
