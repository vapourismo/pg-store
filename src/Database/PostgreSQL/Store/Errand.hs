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

	prepare,
	executePrepWith,
	executePrep,
	queryPrepWith,

	-- insert,
	-- insertMany,
	-- deleteAll,
	-- findAll
) where

import           Control.Applicative
import           Control.Monad.Trans
import           Control.Monad.Except
import           Control.Monad.Reader

import           Data.Maybe

import qualified Data.ByteString           as B

import           Data.Attoparsec.ByteString.Char8

import qualified Database.PostgreSQL.LibPQ as P

import           Database.PostgreSQL.Store.Types
import           Database.PostgreSQL.Store.Value
import           Database.PostgreSQL.Store.Parameters
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
	| Multiple [ErrandError]
	deriving (Show, Eq)

instance Monoid ErrandError where
	mempty = UserError "mempty"

	mappend (Multiple l) (Multiple r) = Multiple (l ++ r)
	mappend (Multiple l) r            = Multiple (l ++ [r])
	mappend l            (Multiple r) = Multiple (l : r)
	mappend l            r            = Multiple [l, r]

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
runErrand con (Errand errand) =
	runExceptT (runReaderT errand con)

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

-- | Extract the result.
transformResult :: Maybe P.Result -> Errand P.Result
transformResult =
	maybe (throwError NoResult) pure

-- | Execute the query and return its internal result.
execute :: Query -> Errand P.Result
execute (Query statement params) = do
	con <- Errand ask
	mbRes <- liftIO (P.execParams con statement (map transformParam params) P.Text)
	res <- transformResult mbRes
	res <$ validateResult res
	where
		transformParam Null              = Nothing
		transformParam (Value typ value) = Just (typ, value, P.Text)

-- | Same as 'execute' but instead of a 'P.Result' it returns the number of affected rows.
execute' :: Query -> Errand Int
execute' = countAffectedRows <=< execute

-- | Execute a query and process its result set.
query :: (Entity a) => Query -> Errand [a]
query = queryWith parseEntity

-- | Execute a query and process its result set using the provided 'RowParser'.
queryWith :: RowParser a -> Query -> Errand [a]
queryWith parser qry = do
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

-- | Prepare a preparable query.
prepare :: PrepQuery a -> Errand ()
prepare (PrepQuery name stmt _) = do
	con <- Errand ask
	mbRes <- liftIO (P.prepare con name stmt Nothing)
	res <- transformResult mbRes
	validateResult res

-- |
executePrepWith :: (Constructible ts (Errand r))
                => (P.Result -> Errand r) -> PrepQuery (Parameters ts) -> FunctionType ts (Errand r)
executePrepWith end (PrepQuery name _ gens) =
	constructWithParams $ \ params -> do
		con <- Errand ask
		mbRes <- liftIO (P.execPrepared con name (map transformParam (gens params)) P.Text)
		res <- transformResult mbRes
		validateResult res
		end res
	where
		transformParam Null              = Nothing
		transformParam (Value _ value) = Just (value, P.Text)

-- |
executePrep :: (Constructible ts (Errand P.Result))
            => PrepQuery (Parameters ts) -> FunctionType ts (Errand P.Result)
executePrep = executePrepWith pure

-- |
queryPrepWith :: (Constructible ts (Errand [a]))
              => RowParser a -> PrepQuery (Parameters ts) -> FunctionType ts (Errand [a])
queryPrepWith parser =
	executePrepWith $ \ result ->
		Errand (lift (withExceptT ParseError (parseResult result parser)))

-- -- |
-- queryPrep :: (Entity a, Constructible ts (Errand [a]) => PrepQuery (Parameters ts) -> FunctionType ts (Errand [a])
-- queryPrep = undefined

-- -- | Insert a row into a 'Table'.
-- insert :: forall a. (TableEntity a) => a -> Errand Bool
-- insert row = do
-- 	fmap (> 0) . execute' $ buildQuery $ do
-- 		insertCode "INSERT INTO "
-- 		insertName name
-- 		insertCode "("
-- 		insertCommaSeperated (map insertName cols)
-- 		insertCode ") VALUES ("
-- 		insertEntity row
-- 		insertCode ")"
-- 	where
-- 		Table name cols = untag (describeTableType @a)

-- -- | Insert many rows into a 'Table'.
-- insertMany :: forall a. (TableEntity a) => [a] -> Errand Int
-- insertMany [] = pure 0
-- insertMany rows =
-- 	execute' $ buildQuery $ do
-- 		insertCode "INSERT INTO "
-- 		insertName name
-- 		insertCode "("
-- 		insertCommaSeperated (map insertName cols)
-- 		insertCode ") VALUES "
-- 		insertCommaSeperated (map insertRowValue rows)
-- 	where
-- 		Table name cols = untag (describeTableType @a)

-- 		insertRowValue row = do
-- 			insertCode "("
-- 			insertEntity row
-- 			insertCode ")"

-- -- | Delete all rows of a 'Table'.
-- deleteAll :: forall a proxy. (TableEntity a) => proxy a -> Errand Int
-- deleteAll _ =
-- 	execute' $ buildQuery $ do
-- 		insertCode "DELETE FROM "
-- 		insertName (tableName (untag (describeTableType @a)))

-- -- | Find every row of a 'Table'.
-- findAll :: forall a. (TableEntity a) => Errand [a]
-- findAll =
-- 	query $ buildQuery $ do
-- 		insertCode "SELECT "
-- 		insertColumns tableDesc
-- 		insertCode " FROM "
-- 		insertName (tableName tableDesc)
-- 	where
-- 		tableDesc = untag (describeTableType @a)
