{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

-- |
-- Module:     Database.PostgreSQL.Store.Errand
-- Copyright:  (c) Ole Krüger 2015-2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.Errand (
	-- * Errand
	ErrandError (..),
	ErrorCode (..),
	Errand,

	runErrand,

	execute,
	query,
	queryWith
) where

import           Control.Monad.Trans
import           Control.Monad.Except
import           Control.Monad.Reader

import           Data.Maybe
import qualified Data.ByteString           as B

import qualified Database.PostgreSQL.LibPQ as P

import           Database.PostgreSQL.Store.Types
import           Database.PostgreSQL.Store.RowParser
import           Database.PostgreSQL.Store.Entity

-- | Error during errand
data ErrandError
	= NoResult
		-- ^ No 'Result' has been returned.
	| UserError String
		-- ^ A user has thrown an error.
	| ExecError P.ExecStatus ErrorCode B.ByteString B.ByteString B.ByteString
		-- ^ Query execution failed.
	| ParseError RowError
		-- ^ Result processing failed.
	deriving (Show, Eq)

-- | Error codes
data ErrorCode
	= UnknownErrorCause B.ByteString
	| IntegrityViolation
	| RestrictViolation
	| NotNullViolation
	| ForeignKeyViolation
	| UniqueViolation
	| CheckViolation
	| ExclusionViolation
	deriving (Show, Eq)

-- | An interaction with the database
newtype Errand a = Errand (ReaderT P.Connection (ExceptT ErrandError IO) a)
	deriving (Functor, Applicative, Monad, MonadIO, MonadError ErrandError)

-- | Run an errand.
runErrand :: P.Connection -> Errand a -> IO (Either ErrandError a)
runErrand con (Errand errand) =
	runExceptT (runReaderT errand con)

-- | Execute a query and return the internal raw result.
execute :: Query a -> Errand P.Result
execute (Query statement params) = Errand . ReaderT $ \ con -> do
	res <- ExceptT (transformResult <$> P.execParams con statement (map transformParam params) P.Text)
	status <- lift (P.resultStatus res)

	case status of
		P.CommandOk   -> pure res
		P.TuplesOk    -> pure res
		P.SingleTuple -> pure res

		other -> do
			(state, msg, detail, hint) <- lift $
				(,,,) <$> P.resultErrorField res P.DiagSqlstate
				      <*> P.resultErrorField res P.DiagMessagePrimary
				      <*> P.resultErrorField res P.DiagMessageDetail
				      <*> P.resultErrorField res P.DiagMessageHint

			let cause =
				case fromMaybe B.empty state of
					"23000" -> IntegrityViolation
					"23001" -> RestrictViolation
					"23502" -> NotNullViolation
					"23503" -> ForeignKeyViolation
					"23505" -> UniqueViolation
					"23514" -> CheckViolation
					"23P01" -> ExclusionViolation
					code    -> UnknownErrorCause code

			throwError (ExecError other
			                      cause
			                      (fromMaybe B.empty msg)
			                      (fromMaybe B.empty detail)
			                      (fromMaybe B.empty hint))

	where
		-- Turn 'Maybe P.Result' into 'Either ErrandError P.Result'
		transformResult =
			maybe (throwError NoResult) pure

		-- Turn 'TypedValue' into 'Maybe (P.Oid, B.ByteString, P.Format)'
		transformParam (TypedValue typ mbValue) =
			(\ (Value value) -> (typ, value, P.Text)) <$> mbValue

-- | Execute a query and process its result set.
query :: (Entity a) => Query a -> Errand [a]
query qry =
	queryWith qry parseEntity

-- | Execute a query and process its result set using the provided 'RowParser'.
queryWith :: Query a -> RowParser a -> Errand [a]
queryWith qry parser = do
	result <- execute qry
	Errand (lift (withExceptT ParseError (parseResult result parser)))
