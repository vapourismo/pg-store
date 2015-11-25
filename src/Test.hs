{-# LANGUAGE OverloadedStrings, TemplateHaskell, RecordWildCards #-}

module Test where

import           Control.Monad
import           Data.Typeable
import qualified Data.ByteString           as B
import qualified Database.PostgreSQL.LibPQ as P
import           Database.PostgreSQL.Store.TH
import           Database.PostgreSQL.Store.Types

data Movie = Movie {
	title :: B.ByteString,
	year  :: Int
} deriving Show

makeTable ''Movie

executeStatement :: P.Connection -> Statement -> IO (Maybe P.Result)
executeStatement con Statement {..} =
	P.execParams con statementContent (map makeParam statementParams) P.Text
	where
		makeParam Value {..} =
			Just (valueType, valueData, valueFormat)

printResultDetail :: P.Result -> IO ()
printResultDetail result = do
	--rows <- P.ntuples result
	cols <- P.nfields result

	forM_ [0 .. cols - 1] $ \ col -> do
		mbName <- P.fname result col
		format <- P.fformat result col
		typ <- P.ftype result col

		putStr (show (maybe "<none>" id mbName))
		putStr " "
		putStr (show format)
		putStr " "
		putStrLn (show typ)

test :: IO ()
test = do
	con <- P.connectdb "postgres://localhost/ole"
	P.status con >>= print

	mbRes1 <- executeStatement con (createStatement (Proxy :: Proxy Movie))
	maybe (P.status con >>= print) printResultDetail mbRes1

	mbRes2 <- executeStatement con (insertStatement (Movie "Test Movie" 2000))
	maybe (P.status con >>= print) printResultDetail mbRes2

	P.finish con
