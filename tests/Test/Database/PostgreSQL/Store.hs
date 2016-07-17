module Test.Database.PostgreSQL.Store (
	allSpecs
) where

import Test.Hspec
import Test.Database.PostgreSQL.Store.Query

allSpecs :: Spec
allSpecs =
	allQuerySpecs
