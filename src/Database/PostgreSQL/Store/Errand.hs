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
	queryWith,

	insert,
	insertMany,
	deleteAll,
	findAll
) where

import           Control.Monad.Trans
import           Control.Monad.Except
import           Control.Monad.Reader

import           Data.Proxy
import           Data.Maybe
import qualified Data.ByteString           as B

import           Data.Attoparsec.ByteString.Char8

import qualified Database.PostgreSQL.LibPQ as P

import           Database.PostgreSQL.Store.Types
import           Database.PostgreSQL.Store.Table
import           Database.PostgreSQL.Store.Entity
import           Database.PostgreSQL.Store.RowParser
import           Database.PostgreSQL.Store.Query.Builder

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

-- | Same as 'execute' but instead of a 'P.Result' it returns the number of affected rows.
execute' :: Query a -> Errand Int
execute' =
	countAffectedRows <=< execute

-- | Execute a query and process its result set.
query :: (Entity a) => Query a -> Errand [a]
query qry =
	queryWith qry parseEntity

-- | Execute a query and process its result set using the provided 'RowParser'.
queryWith :: Query a -> RowParser a -> Errand [a]
queryWith qry parser = do
	result <- execute qry
	Errand (lift (withExceptT ParseError (parseResult result parser)))

-- | Counts the rows that have been affected by a query.
countAffectedRows :: P.Result -> Errand Int
countAffectedRows res = do
	fmap (\ numTuples -> fromMaybe 0 (numTuples >>= maybeResult . endResult . parse decimal))
	     (liftIO (P.cmdTuples res))
	where
		endResult (Partial f) = f B.empty
		endResult x           = x

-- | Insert a row into a 'Table'.
insert :: (TableEntity a) => a -> Errand Bool
insert row = do
	fmap (> 0) . execute' $ buildQuery $ do
		insertCode "INSERT INTO "
		insertName name
		insertCode "("
		insertCommaSeperated (map (\ (Column colName _) -> insertName colName) cols)
		insertCode ") VALUES ("
		insertEntity row
		insertCode ")"
	where
		Table name cols =
			describeTableType ((const Proxy :: a -> Proxy a) row)

-- | Insert many rows into a 'Table'.
insertMany :: (TableEntity a) => [a] -> Errand Int
insertMany [] = pure 0
insertMany rows =
	execute' $ buildQuery $ do
		insertCode "INSERT INTO "
		insertName name
		insertCode "("
		insertCommaSeperated (map (\ (Column colName _) -> insertName colName) cols)
		insertCode ") VALUES "
		insertCommaSeperated (map insertRowValue rows)
	where
		Table name cols =
			describeTableType ((const Proxy :: [a] -> Proxy a) rows)

		insertRowValue row = do
			insertCode "("
			insertEntity row
			insertCode ")"

-- | Delete all rows of a 'Table'.
deleteAll :: (TableEntity a) => proxy a -> Errand Int
deleteAll proxy =
	execute' $ buildQuery $ do
		insertCode "DELETE FROM "
		insertName (tableName (describeTableType proxy))

-- | Find every row of a 'Table'.
findAll :: (TableEntity a) => Errand [a]
findAll =
	query (findAllQuery Proxy)
	where
		findAllQuery :: (TableEntity a) => Proxy a -> Query a
		findAllQuery proxy =
			buildQuery $ do
				insertCode "SELECT "
				insertColumns (describeTableType proxy)
				insertCode " FROM "
				insertName (tableName (describeTableType proxy))
