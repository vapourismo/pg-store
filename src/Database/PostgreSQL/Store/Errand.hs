{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, FlexibleInstances,
             UndecidableInstances, QuasiQuotes, RecordWildCards #-}

-- |
-- Module:     Database.PostgreSQL.Store.Errand
-- Copyright:  (c) Ole Krüger 2015-2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.Errand (
	-- * Errand
	ErrandError (..),
	ErrorCode (..),
	P.ExecStatus (..),

	Errand,
	runErrand,

	execute,
	query,
	query_,
	queryWith,

	-- * Result parser
	Result (..),

	-- * Reference
	Reference (..),

	-- * Queries
	insert,
	insertMany
) where

import           Control.Monad.Trans
import           Control.Monad.Except
import           Control.Monad.Reader

import           Data.Int
import           Data.Maybe
import           Data.Proxy
import qualified Data.ByteString           as B

import qualified Database.PostgreSQL.LibPQ as P

import           Database.PostgreSQL.Store.Query
import           Database.PostgreSQL.Store.Query.Builder
import           Database.PostgreSQL.Store.Table.Class
import           Database.PostgreSQL.Store.Result
import           Database.PostgreSQL.Store.Columns

-- | Error during errand
data ErrandError
	= NoResult
		-- ^ No 'Result' has been returned.
	| EmptyResult
		-- ^ Result set is empty.
	| UserError String
		-- ^ A user has thrown an error.
	| ExecError P.ExecStatus ErrorCode B.ByteString B.ByteString B.ByteString
		-- ^ Query execution failed.
	| ResultError ResultError
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
	res <- ExceptT $
		transformResult <$> P.execParams con statement (map transformParam params) P.Text
	status <- lift (P.resultStatus res)

	case status of
		P.CommandOk -> pure res
		P.TuplesOk  -> pure res

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
		transformResult = maybe (throwError NoResult) pure

		-- Turn 'Value' into 'Maybe (P.Oid, B.ByteString, P.Format)'
		transformParam (Value typ dat) = Just (typ, dat, P.Text)
		transformParam NullValue       = Nothing

-- | Allows you to implement a custom result parser for your type.
--   'mkTable' can implement this for your type.
class Result a where
	queryResultProcessor :: ResultProcessor a

-- | Every type that implements 'Column' can also implement 'Result'.
instance {-# OVERLAPPABLE #-} (Column a) => Result a where
	queryResultProcessor =
		unpackColumn

-- | Combine result parsers sequencially.
instance (Result a, Result b) => Result (a, b) where
	queryResultProcessor =
		(,) <$> queryResultProcessor <*> queryResultProcessor

instance (Result a, Result b, Result c) => Result (a, b, c) where
	queryResultProcessor =
		(,,) <$> queryResultProcessor <*> queryResultProcessor <*> queryResultProcessor

instance (Result a, Result b, Result c, Result d) => Result (a, b, c, d) where
	queryResultProcessor =
		(,,,) <$> queryResultProcessor
		      <*> queryResultProcessor
		      <*> queryResultProcessor
		      <*> queryResultProcessor

instance (Result a, Result b, Result c, Result d, Result e) => Result (a, b, c, d, e) where
	queryResultProcessor =
		(,,,,) <$> queryResultProcessor
		       <*> queryResultProcessor
		       <*> queryResultProcessor
		       <*> queryResultProcessor
		       <*> queryResultProcessor

instance (Result a, Result b, Result c, Result d, Result e, Result f)
         => Result (a, b, c, d, e, f) where
	queryResultProcessor =
		(,,,,,) <$> queryResultProcessor
		        <*> queryResultProcessor
		        <*> queryResultProcessor
		        <*> queryResultProcessor
		        <*> queryResultProcessor
		        <*> queryResultProcessor

instance (Result a, Result b, Result c, Result d, Result e, Result f, Result g)
         => Result (a, b, c, d, e, f, g) where
	queryResultProcessor =
		(,,,,,,) <$> queryResultProcessor
		         <*> queryResultProcessor
		         <*> queryResultProcessor
		         <*> queryResultProcessor
		         <*> queryResultProcessor
		         <*> queryResultProcessor
		         <*> queryResultProcessor

-- | Execute a query and process its result set.
query :: (Result a) => Query a -> Errand [a]
query qry =
	queryWith qry queryResultProcessor

-- | Execute a query and dismiss its result.
query_ :: Query a -> Errand ()
query_ qry =
	() <$ execute qry

-- | Execute a query and process its result set using the provided result processor.
queryWith :: Query a -> ResultProcessor a -> Errand [a]
queryWith qry proc = do
	result <- execute qry
	Errand (lift (withExceptT ResultError (processResult result proc)))

-- | Reference a row of type @a@.
newtype Reference a = Reference Int64
	deriving (Eq, Ord)

instance Show (Reference a) where
	show (Reference n) = show n

instance (Table a) => Column (Reference a) where
	pack (Reference rid) =
		pack rid

	unpack val =
		Reference <$> unpack val

	columnInfo proxy =
		defaultColumnInfo {
			columnTypeName =
				[pgsq| BIGINT REFERENCES ${insertTableName tableProxy}
				                         (${insertTableColumnNames tableProxy}) |]
		}
		where
			tableProxy =
				(const Proxy :: proxy (Reference a) -> Proxy a) proxy

instance Result (Reference a) where
	queryResultProcessor =
		Reference <$> unpackColumn

-- | Insert a single row into a table. Returns the inserted value of the identifier column.
insert :: (Table a) => a -> Query (Reference a)
insert row =
	[pgsq| INSERT INTO @row (#row) VALUES (${unpackRow row}) RETURNING &row |]

-- | Insert many rows into a table.
insertMany :: (Table a) => [a] -> Query (Reference a)
insertMany [] = Query B.empty []
insertMany rows =
	[pgsq| INSERT INTO ${insertTableName rows}
	       VALUES ${map (surroundWithParens . insertEntity . unpackRow) rows}
	       RETURNING ${insertTableIdentColumnName rows} |]
