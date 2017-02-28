{-# LANGUAGE OverloadedStrings,
             GeneralizedNewtypeDeriving,
             TypeApplications,
             TypeFamilies,
             ScopedTypeVariables,
             MultiParamTypeClasses,
             FunctionalDependencies,
             UndecidableInstances,
             FlexibleInstances,
             FlexibleContexts,
             TypeSynonymInstances,
             DataKinds #-}

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
	execute',
	query,
	queryWith,

	prepare
) where

import           GHC.TypeLits

import           Control.Applicative
import           Control.Monad.Trans
import           Control.Monad.Except
import           Control.Monad.Reader

import           Data.Maybe

import qualified Data.ByteString           as B

import           Data.Attoparsec.ByteString.Char8

import qualified Database.PostgreSQL.LibPQ as P

import           Database.PostgreSQL.Store.Types
import           Database.PostgreSQL.Store.Tuple
import           Database.PostgreSQL.Store.Entity
import           Database.PostgreSQL.Store.RowParser

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

instance Monoid ErrandError where
	mempty = UserError "mempty"

	mappend _ e = e

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
	deriving (Functor, Applicative, Monad, Alternative, MonadIO, MonadError ErrandError)

-- | Run an errand.
runErrand :: P.Connection -> Errand a -> IO (Either ErrandError a)
runErrand con (Errand errand) = runExceptT (runReaderT errand con)

-- | Validate the result.
validateResult :: P.Result -> Errand ()
validateResult res = do
	status <- liftIO (P.resultStatus res)

	case status of
		P.CommandOk   -> pure ()
		P.TuplesOk    -> pure ()
		P.SingleTuple -> pure ()

		other -> do
			(state, msg, detail, hint) <- liftIO $
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

-- | Counts the rows that have been affected by a query.
countAffectedRows :: P.Result -> Errand Int
countAffectedRows res = do
	fmap (\ numTuples -> fromMaybe 0 (numTuples >>= maybeResult . endResult . parse decimal))
	     (liftIO (P.cmdTuples res))
	where
		endResult (Partial f) = f B.empty
		endResult x           = x

-- | Extract the result.
transformResult :: Maybe P.Result -> Errand P.Result
transformResult = maybe (throwError NoResult) pure

-- | Identifies @q@ as a query object.
class ErrandQuery q r where
	-- | A type equal to @Errand r@ or a function which will eventually yield a @Errand r@
	type ErrandResult q r

	-- | Execute the query described in @q x@ and pass its 'P.Result' to the given function.
	executeWith :: (P.Result -> Errand r) -> q x -> ErrandResult q r

instance ErrandQuery Query r where
	type ErrandResult Query r = Errand r

	executeWith end (Query stmt params) = do
		con <- Errand ask
		mbRes <- liftIO (P.execParams con stmt params P.Text)
		res <- transformResult mbRes
		validateResult res
		end res

instance (WithTuple ts (Errand r)) => ErrandQuery (PrepQuery ts) r where
	type ErrandResult (PrepQuery ts) r = FunctionType ts (Errand r)

	executeWith end (PrepQuery name _ _ gens) =
		withTuple $ \ params -> do
			con <- Errand ask
			mbRes <- liftIO (P.execPrepared con name (gens params) P.Text)
			res <- transformResult mbRes
			validateResult res
			end res

-- | Execute the query and return its internal result.
execute :: (ErrandQuery q P.Result) => q r -> ErrandResult q P.Result
execute = executeWith pure

-- | Same as 'execute' but instead of a 'P.Result' it returns the number of affected rows.
execute' :: (ErrandQuery q Int) => q r -> ErrandResult q Int
execute' = executeWith countAffectedRows

-- | Execute a query and process its result set using the provided 'RowParser'.
queryWith :: (ErrandQuery q [r], KnownNat n) => RowParser n r -> q r -> ErrandResult q [r]
queryWith parser =
	executeWith $ \ result ->
		Errand (lift (withExceptT ParseError (processResultWith result parser)))

-- | Execute a query and process its result set.
query :: (EntityC r, ErrandQuery q [r]) => q r -> ErrandResult q [r]
query = queryWith parseEntity

-- | Prepare a preparable query.
prepare :: PrepQuery a r -> Errand ()
prepare (PrepQuery name stmt oids _) = do
	con <- Errand ask
	mbRes <- liftIO (P.prepare con name stmt (Just oids))
	res <- transformResult mbRes
	validateResult res
