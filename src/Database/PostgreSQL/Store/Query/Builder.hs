{-# LANGUAGE OverloadedStrings, RecordWildCards, TypeSynonymInstances, FlexibleInstances #-}

-- |
-- Module:     Database.PostgreSQL.Store.Query.Builder
-- Copyright:  (c) Ole Krüger 2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.Query.Builder (
	-- * Query Builder
	QueryBuilder,
	insertCode,
	insertTypedValue,
	insertValue,
	insertValue',
	insertQuote,
	insertName,

	-- * Generalized Building
	FromQueryBuilder (..)
) where

import           Control.Monad.State.Strict

import qualified Data.ByteString           as B

import           Database.PostgreSQL.LibPQ (invalidOid)

import           Database.PostgreSQL.Store.Types
import           Database.PostgreSQL.Store.Utilities

-- | Internal builder state
data BuilderState = BuilderState {
	queryCode   :: B.ByteString,
	queryIndex  :: Word,
	queryValues :: [TypedValue]
}

-- | Query builder
type QueryBuilder = State BuilderState ()

-- | Insert a piece of SQL.
insertCode :: B.ByteString -> QueryBuilder
insertCode code =
	modify (\ state -> state {queryCode = B.append (queryCode state) code})

-- | Insert a parameter placeholder into the code and attach the typed value to the query.
insertTypedValue :: TypedValue -> QueryBuilder
insertTypedValue typedValue =
	modify $ \ BuilderState {..} ->
		BuilderState {
			queryCode = B.concat [queryCode, B.singleton 36, showByteString queryIndex],
			queryIndex = queryIndex + 1,
			queryValues = queryValues ++ [typedValue]
		}

-- | Same as 'insertTypedValue' but untyped.
insertValue :: Value -> QueryBuilder
insertValue value =
	insertTypedValue (TypedValue invalidOid (Just value))

-- | Extension of 'insertValue' which will add a type hint to the parameter placeholder.
insertValue' :: B.ByteString -> Value -> QueryBuilder
insertValue' typ value = do
	insertCode "("
	insertValue value
	insertCode " :: "
	insertCode typ
	insertCode ")"

-- | Insert a quote into the code.
insertQuote :: B.ByteString -> QueryBuilder
insertQuote contents =
	insertCode (B.concat [B.singleton 39, -- '
	                      B.concatMap replaceDelim contents,
	                      B.singleton 39])
	where
		replaceDelim 39 = B.pack [39, 39]
		replaceDelim x  = B.singleton x

-- | Insert a name into the code. It will be surrounded by double quotes if necessary.
insertName :: B.ByteString -> QueryBuilder
insertName name =
	if isAllowed then
		insertCode name
	else
		insertCode (B.concat [B.singleton 34, -- "
		                      B.intercalate (B.pack [34, 34]) (B.split 34 name),
		                      B.singleton 34])
	where
		isAllowedHead b =
			(b >= 97 && b <= 122)    -- 'a' to 'z'
			|| (b >= 65 && b <= 90)  -- 'A' to 'Z'
			|| b == 95               -- '_'

		isAllowedBody b =
			isAllowedHead b
			|| (b >= 48 && b <= 57)  -- '0' to '9'

		isAllowed =
			case B.uncons name of
				Nothing -> True
				Just (h, b) -> isAllowedHead h && B.all isAllowedBody b

-- | @a@ can be instantiated using the query builder.
class FromQueryBuilder a where
	buildQuery :: QueryBuilder -> a

instance FromQueryBuilder QueryBuilder where
	buildQuery = id

instance FromQueryBuilder B.ByteString where
	buildQuery builder =
		queryCode (execState builder (BuilderState B.empty 1 []))

instance FromQueryBuilder (B.ByteString, [TypedValue]) where
	buildQuery builder =
		(code, values)
		where BuilderState code _ values = execState builder (BuilderState B.empty 1 [])
