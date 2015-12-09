{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}

module Database.PostgreSQL.Store.QuerySpec (querySpec) where

import Test.Hspec
--import Database.PostgreSQL.Store

querySpec :: Spec
querySpec =
	describe "Table" $
		it "test" $ do
			pure ()
