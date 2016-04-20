{-# LANGUAGE RecordWildCards #-}

-- |
-- Module:     Database.PostgreSQL.Store.Errand
-- Copyright:  (c) Ole Krüger 2015-2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.Errand (
	ErrandError (..),
	Errand,
	runErrand,
	raiseErrandError,
	executeQuery,
	query,
	query_
) where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader

import qualified Data.ByteString           as B

import qualified Database.PostgreSQL.LibPQ as P
import           Database.PostgreSQL.Store.Query
import           Database.PostgreSQL.Store.Result
import           Database.PostgreSQL.Store.Columns

-- | Error during errand
data ErrandError
	= NoResult
	| ExecError P.ExecStatus (Maybe B.ByteString)
	| ResultError ResultError
	| UnexpectedEmptyResult
	| UserError String
	deriving (Show, Eq)

-- | An interaction with the database
type Errand = ReaderT P.Connection (ExceptT ErrandError IO)

-- | Run an errand.
runErrand :: P.Connection -> Errand a -> IO (Either ErrandError a)
runErrand con errand =
	runExceptT (runReaderT errand con)

-- | Raise an error.
raiseErrandError :: ErrandError -> Errand a
raiseErrandError err =
	lift (ExceptT (pure (Left err)))

-- | Execute a query and return its result.
executeQuery :: Query -> Errand P.Result
executeQuery (Query statement params) = do
	con <- ask
	lift $ do
		res <- ExceptT (transformResult <$> P.execParams con statement transformedParams P.Text)
		status <- lift (P.resultStatus res)

		case status of
			P.CommandOk -> pure res
			P.TuplesOk  -> pure res

			other -> do
				msg <- lift (P.resultErrorMessage res)
				throwE (ExecError other msg)

	where
		transformResult =
			maybe (Left NoResult) pure

		transformParam Value {..} = Just (valueType, valueData, valueFormat)
		transformParam NullValue  = Nothing

		transformedParams =
			map transformParam params

-- | Execute a query and process its result set.
-- It is essential that all fields required by the underlying result parser are present.
query :: (Result a) => Query -> Errand [a]
query qry = do
	result <- executeQuery qry
	lift (withExceptT ResultError (runResultProcessor result resultProcessor))

-- | Execute a query.
query_ :: Query -> Errand ()
query_ qry =
	() <$ executeQuery qry

