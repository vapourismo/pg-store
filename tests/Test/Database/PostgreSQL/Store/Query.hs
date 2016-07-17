{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}

module Test.Database.PostgreSQL.Store.Query (
	allQuerySpecs
) where

import Database.PostgreSQL.Store.Query
import Test.Hspec

data TestTable = TestTable
	deriving (Show, Eq, Ord)

instance QueryTable TestTable where
	tableName _      = "T"
	tableIDName _    = "a"
	tableSelectors _ = [SelectorField "b", SelectorField "c"]

allQuerySpecs :: Spec
allQuerySpecs =
	describe "Database.PostgreSQL.Store.Query" $
		describe "pgsq" $ do
			it "resolves table names" $ do
				shouldBe [pgsq|@TestTable|] (Query "\"T\"" [])
				shouldBe [pgsq|@Test.Database.PostgreSQL.Store.Query.TestTable|] [pgsq|@TestTable|]
				shouldBe [pgsq|@TestTable.b|] (Query "\"T\".b" [])
				shouldBe [pgsq|@TestTable."b"|] (Query "\"T\".\"b\"" [])

			it "resolves identifier fields" $ do
				shouldBe [pgsq|&TestTable|] (Query "\"T\".\"a\"" [])
				shouldBe [pgsq|&Test.Database.PostgreSQL.Store.Query.TestTable|] [pgsq|&TestTable|]

			it "resolves selectors" $ do
				shouldBe [pgsq|#TestTable|] (Query "\"T\".\"b\", \"T\".\"c\"" [])
				shouldBe [pgsq|#TestTable|] [pgsq|#Test.Database.PostgreSQL.Store.Query.TestTable|]

			it "ignores special operator use" $ do
				shouldBe [pgsq|1 # 2|]       (Query "1 # 2" [])
				shouldBe [pgsq|a # b|]       (Query "a # b" [])
				shouldBe [pgsq|a # T|]       (Query "a # T" [])
				shouldBe [pgsq|a # T.a|]     (Query "a # T.a" [])
				shouldBe [pgsq|a # "T"|]     (Query "a # \"T\"" [])
				shouldBe [pgsq|a # "T"."a"|] (Query "a # \"T\".\"a\"" [])
				shouldBe [pgsq|1#2|]         (Query "1#2" [])
				shouldBe [pgsq|a#b|]         (Query "a#b" [])
				shouldBe [pgsq|a#(T)|]       (Query "a#(T)" [])
				shouldBe [pgsq|a#(T.a)|]     (Query "a#(T.a)" [])
				shouldBe [pgsq|a#"T"|]       (Query "a#\"T\"" [])
				shouldBe [pgsq|a#"T"."a"|]   (Query "a#\"T\".\"a\"" [])

				shouldBe [pgsq|1 & 2|]       (Query "1 & 2" [])
				shouldBe [pgsq|a & b|]       (Query "a & b" [])
				shouldBe [pgsq|a & T|]       (Query "a & T" [])
				shouldBe [pgsq|a & T.a|]     (Query "a & T.a" [])
				shouldBe [pgsq|a & "T"|]     (Query "a & \"T\"" [])
				shouldBe [pgsq|a & "T"."a"|] (Query "a & \"T\".\"a\"" [])
				shouldBe [pgsq|1&2|]         (Query "1&2" [])
				shouldBe [pgsq|a&b|]         (Query "a&b" [])
				shouldBe [pgsq|a&(T)|]       (Query "a&(T)" [])
				shouldBe [pgsq|a&(T.a)|]     (Query "a&(T.a)" [])
				shouldBe [pgsq|a&"T"|]       (Query "a&\"T\"" [])
				shouldBe [pgsq|a&"T"."a"|]   (Query "a&\"T\".\"a\"" [])

				shouldBe [pgsq|@-5|]      (Query "@-5" [])
				shouldBe [pgsq|@(-5)|]    (Query "@(-5)" [])
				shouldBe [pgsq|@a|]       (Query "@a" [])
				shouldBe [pgsq|@(a)|]     (Query "@(a)" [])
				shouldBe [pgsq|@(T)|]     (Query "@(T)" [])
				shouldBe [pgsq|@(T.a)|]   (Query "@(T.a)" [])
				shouldBe [pgsq|@"a"|]     (Query "@\"a\"" [])
				shouldBe [pgsq|@"T"|]     (Query "@\"T\"" [])
				shouldBe [pgsq|@"T"."a"|] (Query "@\"T\".\"a\"" [])

				shouldBe [pgsq|$1|]       (Query "$1" [])
				shouldBe [pgsq|$(1)|]     (Query "$(1)" [])
				shouldBe [pgsq|$(a)|]     (Query "$(a)" [])
				shouldBe [pgsq|$(T)|]     (Query "$(T)" [])
				shouldBe [pgsq|$(T.a)|]   (Query "$(T.a)" [])
				shouldBe [pgsq|$"a"|]     (Query "$\"a\"" [])
				shouldBe [pgsq|$"T"|]     (Query "$\"T\"" [])
				shouldBe [pgsq|$"T"."a"|] (Query "$\"T\".\"a\"" [])
