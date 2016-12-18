-- |
-- Module:     Database.PostgreSQL.Store.Result
-- Copyright:  (c) Ole Krüger 2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.Result (
	-- * Exported modules
	module Database.PostgreSQL.Store.RowParser,
	module Database.PostgreSQL.Store.Entity,

	-- * Processor
	ResultProcessError,
	processResultWith,
	processResult
) where

import           Control.Monad.Except

import           Database.PostgreSQL.Store.RowParser
import           Database.PostgreSQL.Store.Entity

import qualified Database.PostgreSQL.LibPQ as P

-- | Error that occurs during result processing.
data ResultProcessError
	= ParseError RowError
		-- ^ An error occured during parsing of the result.
	| UnsupportedStatus P.ExecStatus
		-- ^ 'P.Result' has an unsupported result status.
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
	withExceptT ParseError (parseResult result parser)

-- | Process the 'P.Result' with a 'RowParser' provided by the 'ResultEntity' instance.
processResult :: (Entity a) => P.Result -> ExceptT ResultProcessError IO [a]
processResult result =
	processResultWith result parseEntity
