{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

-- |
-- Module:     Database.PostgreSQL.Store.Errand
-- Copyright:  (c) Ole Krüger 2015-2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.Errand (
	ErrandError (..),
	Errand,
	runErrand,

	execute,
	query,
	query_,

	Result (..),

	Single (..)
) where

import           Control.Monad.Trans
import           Control.Monad.Except
import           Control.Monad.Reader

import           Data.Maybe
import qualified Data.ByteString           as B

import qualified Database.PostgreSQL.LibPQ as P
import           Database.PostgreSQL.Store.Query
import           Database.PostgreSQL.Store.Result
import           Database.PostgreSQL.Store.Columns

-- | Error during errand
data ErrandError
	= NoResult
	| EmptyResult
	| UserError String
	| ExecError P.ExecStatus (Maybe B.ByteString)
	| ResultError ResultError
	| IntegrityViolation B.ByteString B.ByteString
	| RestrictViolation B.ByteString B.ByteString
	| NotNullViolation B.ByteString B.ByteString
	| ForeignKeyViolation B.ByteString B.ByteString
	| UniqueViolation B.ByteString B.ByteString
	| CheckViolation B.ByteString B.ByteString
	| ExclusionViolation B.ByteString B.ByteString
	deriving (Show, Eq)

-- | An interaction with the database
newtype Errand a = Errand (ReaderT P.Connection (ExceptT ErrandError IO) a)
	deriving (Functor, Applicative, Monad, MonadIO, MonadError ErrandError)

-- | Run an errand.
runErrand :: P.Connection -> Errand a -> IO (Either ErrandError a)
runErrand con (Errand errand) =
	runExceptT (runReaderT errand con)

-- | Execute a query and return its result.
execute :: Query -> Errand P.Result
execute (Query statement params) = Errand . ReaderT $ \ con -> do
	res <- ExceptT (transformResult <$> P.execParams con statement transformedParams P.Text)
	status <- lift (P.resultStatus res)

	case status of
		P.CommandOk -> pure res
		P.TuplesOk  -> pure res

		other -> do
			info <- lift $
				(,,,,,,,) <$> P.resultErrorField res P.DiagSqlstate
				          <*> P.resultErrorField res P.DiagMessagePrimary
				          <*> P.resultErrorField res P.DiagMessageDetail
				          <*> P.resultErrorField res P.DiagMessageHint
				          <*> P.resultErrorField res P.DiagStatementPosition
				          <*> P.resultErrorField res P.DiagInternalPosition
				          <*> P.resultErrorField res P.DiagInternalQuery
				          <*> P.resultErrorField res P.DiagContext

			case info of
				(Just "23000", msg, detail, _, _, _, _, _) ->
					throwError (IntegrityViolation (fromMaybe B.empty msg)
					                               (fromMaybe B.empty detail))

				(Just "23001", msg, detail, _, _, _, _, _) ->
					throwError (RestrictViolation (fromMaybe B.empty msg)
					                              (fromMaybe B.empty detail))

				(Just "23502", msg, detail, _, _, _, _, _) ->
					throwError (NotNullViolation (fromMaybe B.empty msg)
					                             (fromMaybe B.empty detail))

				(Just "23503", msg, detail, _, _, _, _, _) ->
					throwError (ForeignKeyViolation (fromMaybe B.empty msg)
					                                (fromMaybe B.empty detail))

				(Just "23505", msg, detail, _, _, _, _, _) ->
					throwError (UniqueViolation (fromMaybe B.empty msg)
					                            (fromMaybe B.empty detail))

				(Just "23514", msg, detail, _, _, _, _, _) ->
					throwError (CheckViolation (fromMaybe B.empty msg)
					                           (fromMaybe B.empty detail))

				(Just "23P01", msg, detail, _, _, _, _, _) ->
					throwError (ExclusionViolation (fromMaybe B.empty msg)
					                               (fromMaybe B.empty detail))

				_ -> do
					msg <- lift (P.resultErrorMessage res)
					throwError (ExecError other msg)

	where
		transformResult = maybe (throwError NoResult) pure

		transformParam (Value typ dat fmt) = Just (typ, dat, fmt)
		transformParam NullValue           = Nothing

		transformedParams = map transformParam params

-- | Allows you to implement a custom result parser for queries.
class Result a where
	queryResultProcessor :: ResultProcessor a

instance (Result a, Result b) => Result (a, b) where
	queryResultProcessor =
		(,) <$> queryResultProcessor <*> queryResultProcessor

instance (Result a, Result b, Result c) => Result (a, b, c) where
	queryResultProcessor =
		(,,) <$> queryResultProcessor <*> queryResultProcessor <*> queryResultProcessor

instance (Result a, Result b, Result c, Result d) => Result (a, b, c, d) where
	queryResultProcessor =
		(,,,) <$> queryResultProcessor <*> queryResultProcessor <*> queryResultProcessor <*> queryResultProcessor

instance (Result a, Result b, Result c, Result d, Result e) => Result (a, b, c, d, e) where
	queryResultProcessor =
		(,,,,) <$> queryResultProcessor <*> queryResultProcessor <*> queryResultProcessor <*> queryResultProcessor <*> queryResultProcessor

instance (Result a, Result b, Result c, Result d, Result e, Result f) => Result (a, b, c, d, e, f) where
	queryResultProcessor =
		(,,,,,) <$> queryResultProcessor <*> queryResultProcessor <*> queryResultProcessor <*> queryResultProcessor <*> queryResultProcessor <*> queryResultProcessor

instance (Result a, Result b, Result c, Result d, Result e, Result f, Result g) => Result (a, b, c, d, e, f, g) where
	queryResultProcessor =
		(,,,,,,) <$> queryResultProcessor <*> queryResultProcessor <*> queryResultProcessor <*> queryResultProcessor <*> queryResultProcessor <*> queryResultProcessor <*> queryResultProcessor

-- | Execute a query and process its result set.
query :: (Result a) => Query -> Errand [a]
query qry = do
	result <- execute qry
	Errand (lift (withExceptT ResultError (processResult result queryResultProcessor)))

-- | Execute a query.
query_ :: Query -> Errand ()
query_ qry =
	() <$ execute qry

-- | Helper type to capture an unassociated column.
newtype Single a = Single { fromSingle :: a }
	deriving (Eq, Ord)

instance (Show a) => Show (Single a) where
	show = show . fromSingle

instance (Column a) => Result (Single a) where
	queryResultProcessor = Single <$> unpackColumn
