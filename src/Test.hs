{-# LANGUAGE OverloadedStrings, TemplateHaskell, RecordWildCards #-}

module Test where

import           Control.Monad
import           Control.Monad.Trans.Maybe
import qualified Data.ByteString           as B
import qualified Database.PostgreSQL.LibPQ as P
import           Database.PostgreSQL.Store.TH
import           Database.PostgreSQL.Store.Types

data Movie = Movie {
	title :: B.ByteString,
	year  :: Int
} deriving Show

mkTable ''Movie

executeStatement :: P.Connection -> Statement -> IO (Maybe P.Result)
executeStatement con Statement {..} =
	P.execParams con statementContent (map makeParam statementParams) P.Text
	where
		makeParam Value {..} =
			Just (valueType, valueData, valueFormat)

printResultDetail :: P.Result -> IO ()
printResultDetail result = do
	rows <- P.ntuples result
	cols <- P.nfields result

	forM_ [0 .. rows - 1] $ \ row -> do
		forM_ [0 .. cols - 1] $ \ col -> do
			mbName <- P.fname result col
			value <- P.getvalue' result row col
			format <- P.fformat result col

			print (row, col, format, mbName, value)

disectResult :: P.Result -> IO (Maybe [Reference Movie])
disectResult res =
	runMaybeT (fromResult res)

test :: IO ()
test = do
	con <- P.connectdb "postgres://localhost/ole"
	P.status con >>= print

	executeStatement con $(mkCreateStatement ''Movie)

	executeStatement con (insertStatement (Movie "Test Movie 1" 2001))
	executeStatement con (insertStatement (Movie "Test Movie 2" 2002))
	executeStatement con (insertStatement (Movie "Test Movie 3" 2003))
	executeStatement con (insertStatement (Movie "Test Movie 4" 2004))

	P.exec con "SELECT * FROM \"Test.Movie\""
		>>= maybe (P.status con >>= print) (print <=< disectResult)

	executeStatement con $(mkDropStatement ''Movie)

	P.finish con
