{-# LANGUAGE OverloadedStrings, FlexibleInstances, UndecidableInstances #-}

-- |
-- Module:     Database.PostgreSQL.Store.Table.Builder
-- Copyright:  (c) Ole Krüger 2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.Query.Builder (
	-- * Builder
	QueryBuilder,
	insertCode,
	insertName,
	QueryEntity (..),
	insertTableName,
	insertTableIdentColumnName,
	insertTableColumnNames
) where

import           Control.Monad.State.Strict

import           Data.List
import           Data.String
import qualified Data.ByteString as B

import           Database.PostgreSQL.Store.Columns
import           Database.PostgreSQL.Store.Table.Class

-- | Query builder
type QueryBuilder = State (Int, B.ByteString, [Value]) ()

-- | Generate the placeholder for a parameter.
genParam :: Int -> B.ByteString
genParam index =
	B.append "$" (fromString (show index))

-- | Insert a piece of SQL.
insertCode :: B.ByteString -> QueryBuilder
insertCode otherCode =
	modify $ \ (counter, code, values) ->
		(counter, B.append code otherCode, values)

-- | Insert a name. This function takes care of proper quotation.
insertName :: B.ByteString -> QueryBuilder
insertName name =
	if isAllowed then
		insertCode name
	else
		insertCode (B.concat ["\"", B.intercalate "\"\"" (B.split 34 name), "\""])
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

-- | Insert the table name of a table type.
insertTableName :: (Table a) => proxy a -> QueryBuilder
insertTableName proxy =
	insertName (tableName (tableInfo proxy))

-- | Insert the identifier column name of a table type.
insertTableIdentColumnName :: (Table a) => proxy a -> QueryBuilder
insertTableIdentColumnName proxy = do
	insertTableName proxy
	insertCode "."
	insertName (tableIdentColumn (tableInfo proxy))

-- | Insert a comma-seperated list of columns for a table type.
insertTableColumnNames :: (Table a) => proxy a -> QueryBuilder
insertTableColumnNames proxy =
	sequence_ $
		intersperse (insertCode ",") $
			map insertColName (tableColumns (tableInfo proxy))
	where
		insertColName name = do
			insertTableName proxy
			insertCode "."
			insertName name

-- | Generalize over types that can be either inlined or attached.
class QueryEntity a where
	insertEntity :: a -> QueryBuilder

-- | Every type that implements "Column" can be used as entity.
instance {-# OVERLAPPABLE #-} (Column a) => QueryEntity a where
	insertEntity value =
		modify $ \ (counter, code, values) ->
			(counter + 1, B.append code (genParam counter), values ++ [pack value])

-- | List of values are inserted as tuples.
instance {-# OVERLAPPABLE #-} (QueryEntity a) => QueryEntity [a] where
	insertEntity xs = do
		insertCode "("
		sequence_ $
			intersperse (insertCode ",") $
				map insertEntity xs
		insertCode ")"

-- | We need this instance to
instance QueryEntity [Char] where
	insertEntity string =
		insertEntity (pack string)
