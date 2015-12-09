module Main (main) where

import           Test.Hspec
import           System.Environment

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import           Database.PostgreSQL.Store.ColumnsSpec
import           Database.PostgreSQL.Store.TableSpec
import           Database.PostgreSQL.Store.QuerySpec
import qualified Database.PostgreSQL.LibPQ as P

-- | Test which will only be executed if 'PGINFO' environment variable is set
liveTests :: String -> Spec
liveTests pgInfo = do
	(con, status) <- runIO $ do
		con <- P.connectdb (T.encodeUtf8 (T.pack pgInfo))
		status <- P.status con
		pure (con, status)

	describe "Database connection" $
		it "established" $
			status `shouldBe` P.ConnectionOk

	afterAll_ (P.finish con) (tableSpec con)

-- | Test entry point
main :: IO ()
main = hspec $ do
	columnsSpec
	querySpec

	mbPGInfo <- runIO (lookupEnv "PGINFO")
	case mbPGInfo of
		Just pgInfo ->
			liveTests pgInfo

		Nothing ->
			runIO (putStrLn "Environment variable 'PGINFO' is missing")
