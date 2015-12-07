module Main (main) where

import Test.Hspec
import Database.PostgreSQL.Store.ColumnsSpec

-- | Test entry point
main :: IO ()
main = hspec $ do
	columnsSpec
