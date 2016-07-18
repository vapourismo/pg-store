module Main (main) where

import           Test.Hspec
import           Test.Database.PostgreSQL.Store

import           Data.String

import qualified Database.PostgreSQL.LibPQ as P

import           System.Environment

connectMaybe :: String -> IO (Maybe P.Connection)
connectMaybe info = do
	con <- P.connectdb (fromString info)
	status <- P.status con

	pure $
		if status == P.ConnectionOk then
			Just con
		else
			Nothing

-- | Test entry point
main :: IO ()
main = do
	mbInfo <- lookupEnv "PGINFO"
	mbCon <- maybe (pure Nothing) connectMaybe mbInfo

	hspec (allSpecs mbCon)
