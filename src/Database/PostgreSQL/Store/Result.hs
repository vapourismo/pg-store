-- |
-- Module:     Database.PostgreSQL.Store.Result
-- Copyright:  (c) Ole Krüger 2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.Result (
	-- * Exported modules
	module Database.PostgreSQL.Store.Result.Parser,
	module Database.PostgreSQL.Store.Result.Entity,

	-- * Processor
	ResultProcessError,
	processResultWith,
	processResult
) where

import           Control.Monad.Except

import           Database.PostgreSQL.Store.Result.Parser
import           Database.PostgreSQL.Store.Result.Entity

import qualified Database.PostgreSQL.LibPQ as P

-- | Error that occurs during result processing.
data ResultProcessError
	= ResultParseError ResultParseError
	| UnsupportedStatus P.ExecStatus
	deriving (Show, Eq)

-- | Make sure the given 'P.Result' is valid and ready for processing.
validateResult :: P.Result -> ExceptT ResultProcessError IO ()
validateResult result = do
	status <- liftIO (P.resultStatus result)
	case status of
		P.EmptyQuery  -> pure ()
		P.CommandOk   -> pure ()
		P.TuplesOk    -> pure ()
		P.SingleTuple -> pure ()
		x             -> throwError (UnsupportedStatus x)

		-- TODO: Handle 'BadResponse'
		-- TODO: Handle 'NonfatalError'
		-- TODO: Handle 'FatalError'

-- | Process the 'P.Result' with a user-provided 'RowParser'.
processResultWith :: P.Result -> RowParser a -> ExceptT ResultProcessError IO [a]
processResultWith result parser = do
	validateResult result
	withExceptT ResultParseError (parseResult result parser)

-- | Process the 'P.Result' with a 'RowParser' provided by the 'ResultEntity' instance.
processResult :: (ResultEntity a) => P.Result -> ExceptT ResultProcessError IO [a]
processResult result =
	processResultWith result parseEntity
