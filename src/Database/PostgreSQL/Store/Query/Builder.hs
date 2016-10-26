{-# LANGUAGE OverloadedStrings, FlexibleInstances, UndecidableInstances #-}

-- |
-- Module:     Database.PostgreSQL.Store.Query.Builder
-- Copyright:  (c) Ole Krüger 2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.Query.Builder (
	-- * Builder
	QueryBuilder,

	BuildQuery (..),

	insertCode,
	insertQuote,
	insertName,
	QueryEntity (..),
	insertTableName,
	insertTableIdentColumnName,
	insertTableColumnNames,
	surroundWithParens
) where

import           Control.Monad.State.Strict

import           Data.List
import           Data.String
import qualified Data.ByteString as B

import           Database.PostgreSQL.Store.Columns
import           Database.PostgreSQL.Store.Table.Class

-- | Query builder
type QueryBuilder = State (Int, B.ByteString, [Value]) ()

-- | What can be built using a 'QueryBuilder'.
class BuildQuery a where
	-- | Generate an instance of @a@ using the information given by the 'QueryBuilder'.
	buildQuery :: QueryBuilder -> a

-- | Leave 'QueryBuilder' as is.
instance BuildQuery QueryBuilder where
	buildQuery = id

-- | Generate only the query code.
instance BuildQuery B.ByteString where
	buildQuery builder =
		code
		where
			(_, code, _) =
				execState builder (1, B.empty, [])

-- | Generate both query code and values.
instance BuildQuery (B.ByteString, [Value]) where
	buildQuery builder =
		(code, values)
		where
			(_, code, values) =
				execState builder (1, B.empty, [])

-- | Generate the placeholder for a parameter.
genParam :: Int -> B.ByteString
genParam index =
	B.append "$" (fromString (show index))
{-# INLINE genParam #-}

-- | Insert a piece of SQL code into the query.
insertCode :: B.ByteString -> QueryBuilder
insertCode otherCode =
	modify $ \ (counter, code, values) ->
		(counter, B.append code otherCode, values)

-- | Insert a quote.
insertQuote :: B.ByteString -> QueryBuilder
insertQuote contents =
	insertCode (B.concatMap replaceDelim contents)
	where
		replaceDelim 39 = B.pack [39, 39]
		replaceDelim x  = B.singleton x

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

-- | Insert a comma-seperated list of column names for a table type.
insertTableColumnNames :: (Table a) => proxy a -> QueryBuilder
insertTableColumnNames proxy =
	sequence_ $
		intersperse (insertCode ",") $
			map insertColName (tableColumns (tableInfo proxy))
	where
		insertColName (name, _) = do
			insertTableName proxy
			insertCode "."
			insertName name

-- | Surround the given statement with parentheses.
surroundWithParens :: QueryBuilder -> QueryBuilder
surroundWithParens builder = do
	insertCode "("
	builder
	insertCode ")"
{-# INLINE surroundWithParens #-}

-- | Generalize over types that can be either inlined or attached.
class QueryEntity a where
	-- | Embed an entity into the query.
	insertEntity :: a -> QueryBuilder

-- | Every type that implements 'Column' can be used as entity.
instance {-# OVERLAPPABLE #-} (Column a) => QueryEntity a where
	insertEntity value =
		modify $ \ (counter, code, values) ->
			(counter + 1, B.append code (genParam counter), values ++ [pack value])

-- | Differentiate from @QueryEntity a => QueryEntity [a]@.
instance QueryEntity [Char] where
	insertEntity string =
		insertEntity (pack string)
	{-# INLINE insertEntity #-}

-- | Inline a query builder as is.
instance QueryEntity QueryBuilder where
	insertEntity builder =
		builder
	{-# INLINE insertEntity #-}

-- | List of entities are inserted as a comma-seperated list. Parentheses are not inserted.
instance {-# OVERLAPPABLE #-} (QueryEntity a) => QueryEntity [a] where
	insertEntity xs =
		sequence_ $
			intersperse (insertCode ",") $
				map insertEntity xs

-- | Tuple of entities is inserted as a tuple in SQL.
instance (QueryEntity a, QueryEntity b) => QueryEntity (a, b) where
	insertEntity (a, b) =
		surroundWithParens $ do
			insertEntity a
			insertCode ","
			insertEntity b

-- | Tuple of entities is inserted as a tuple in SQL.
instance (QueryEntity a, QueryEntity b, QueryEntity c) => QueryEntity (a, b, c) where
	insertEntity (a, b, c) =
		surroundWithParens $ do
			insertEntity a
			insertCode ","
			insertEntity b
			insertCode ","
			insertEntity c

-- | Tuple of entities is inserted as a tuple in SQL.
instance (QueryEntity a, QueryEntity b, QueryEntity c, QueryEntity d)
         => QueryEntity (a, b, c, d) where
	insertEntity (a, b, c, d) =
		surroundWithParens $ do
			insertEntity a
			insertCode ","
			insertEntity b
			insertCode ","
			insertEntity c
			insertCode ","
			insertEntity d

-- | Tuple of entities is inserted as a tuple in SQL.
instance (QueryEntity a, QueryEntity b, QueryEntity c, QueryEntity d, QueryEntity e)
         => QueryEntity (a, b, c, d, e) where
	insertEntity (a, b, c, d, e) =
		surroundWithParens $ do
			insertEntity a
			insertCode ","
			insertEntity b
			insertCode ","
			insertEntity c
			insertCode ","
			insertEntity d
			insertCode ","
			insertEntity e

-- | Tuple of entities is inserted as a tuple in SQL.
instance (QueryEntity a, QueryEntity b, QueryEntity c, QueryEntity d, QueryEntity e, QueryEntity f)
         => QueryEntity (a, b, c, d, e, f) where
	insertEntity (a, b, c, d, e, f) =
		surroundWithParens $ do
			insertEntity a
			insertCode ","
			insertEntity b
			insertCode ","
			insertEntity c
			insertCode ","
			insertEntity d
			insertCode ","
			insertEntity e
			insertCode ","
			insertEntity f

-- | Tuple of entities is inserted as a tuple in SQL.
instance (QueryEntity a, QueryEntity b, QueryEntity c, QueryEntity d, QueryEntity e, QueryEntity f,
          QueryEntity g)
         => QueryEntity (a, b, c, d, e, f, g) where
	insertEntity (a, b, c, d, e, f, g) =
		surroundWithParens $ do
			insertEntity a
			insertCode ","
			insertEntity b
			insertCode ","
			insertEntity c
			insertCode ","
			insertEntity d
			insertCode ","
			insertEntity e
			insertCode ","
			insertEntity f
			insertCode ","
			insertEntity g
