module Main (main) where

import Test.Hspec
import Test.Database.PostgreSQL.Store

-- | Test entry point
main :: IO ()
main = hspec allSpecs
