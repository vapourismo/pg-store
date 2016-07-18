module Test.Database.PostgreSQL.Store (
	allSpecs
) where

import           Test.Hspec

import           Test.Database.PostgreSQL.Store.Columns
import           Test.Database.PostgreSQL.Store.Query

import qualified Database.PostgreSQL.LibPQ as P

allSpecs :: Maybe P.Connection -> Spec
allSpecs mbCon = do
	allColumnsSpecs mbCon
	allQuerySpecs
