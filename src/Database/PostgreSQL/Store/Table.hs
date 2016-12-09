{-# LANGUAGE OverloadedStrings,
             ConstraintKinds,
             PolyKinds,
             DataKinds,
             FlexibleContexts,
             FlexibleInstances,
             GADTs,
             TypeFamilies,
             TypeOperators,
             TypeSynonymInstances,
             UndecidableInstances,
             ScopedTypeVariables,
             QuasiQuotes,
             DefaultSignatures
#-}

-- |
-- Module:     Database.PostgreSQL.Store.Schema
-- Copyright:  (c) Ole Krüger 2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.Table (
	-- * Columns
	Column (..),
	ColumnType (..),
	ColumnEntity (..),

	-- * Table
	Table (..),
	TableEntity (..),
	buildTableSchema,

	GenericTable,
	describeGenericTable,

	-- * Helpers
	KColumns (..),
	KTable (..),

	GColumns (..),
	GTable (..),

	AnalyzeRecordRep,
	AnalyzeTableRep,

	AnalyzeTable
) where

import           GHC.Generics
import           GHC.TypeLits

import           Control.Monad

import           Data.Kind
import           Data.Proxy

import qualified Data.ByteString as B

import           Database.PostgreSQL.Store.Query
import           Database.PostgreSQL.Store.Utilities

-- |
data ColumnType = ColumnType {
	colDescTypeName :: B.ByteString,
	colDescNotNull  :: Bool,
	colDescCheck    :: Maybe (B.ByteString -> QueryBuilder)
}

-- | Classify a type which can be used as a column in a table.
class (Entity a) => ColumnEntity a where
	-- |
	describeColumnType :: proxy a -> ColumnType

instance (ColumnEntity a) => ColumnEntity (Maybe a) where
	describeColumnType _ =
		(describeColumnType (Proxy :: Proxy a)) {
			colDescNotNull = False
		}

instance ColumnEntity Int where
	describeColumnType _ =
		ColumnType "int8" True Nothing

instance ColumnEntity String where
	describeColumnType _ =
		ColumnType "text" True Nothing

-- |
data KColumns
	= TCombine KColumns KColumns
	| TSelector Symbol Type

-- |
data Column = Column {
	colName :: B.ByteString,
	colDesc :: ColumnType
}

-- |
class GColumns (sel :: KColumns) where
	-- |
	gDescribeColumns :: proxy sel -> [Column]

instance (KnownSymbol name, ColumnEntity typ) => GColumns ('TSelector name typ) where
	gDescribeColumns _ =
		[Column (buildByteString (symbolVal (Proxy :: Proxy name)))
		        (describeColumnType (Proxy :: Proxy typ))]

instance (GColumns lhs, GColumns rhs) => GColumns ('TCombine lhs rhs) where
	gDescribeColumns _ =
		gDescribeColumns (Proxy :: Proxy lhs)
		++ gDescribeColumns (Proxy :: Proxy rhs)

-- |
type family AnalyzeRecordRep org (sel :: * -> *) :: KColumns where
	-- Single record field
	AnalyzeRecordRep org (S1 ('MetaSel ('Just name) m1 m2 m3) (Rec0 typ)) =
		'TSelector name typ

	-- Non-record field
	AnalyzeRecordRep org (S1 ('MetaSel 'Nothing m1 m2 m3) (Rec0 typ)) =
		TypeError ('Text "Given type "
		           ':<>: 'ShowType org
		           ':<>: 'Text " must have a single record constructor")

	-- Multiple fields
	AnalyzeRecordRep org (lhs :*: rhs) =
		'TCombine (AnalyzeRecordRep org lhs) (AnalyzeRecordRep org rhs)

	-- Missing field(s)
	AnalyzeRecordRep org U1 =
		TypeError ('Text "Given type "
		           ':<>: 'ShowType org
		           ':<>: 'Text " has one constructor, therefore that constructor must have \
		                       \at least one field")

	-- Something else
	AnalyzeRecordRep org other =
		TypeError ('Text "Given type "
		           ':<>: 'ShowType org
		           ':<>: 'Text " has a constructor with an invalid selector"
		           ':$$: 'ShowType other)

-- |
data KTable = TTable Symbol KColumns

-- |
data Table = Table B.ByteString [Column]

-- |
class GTable (tbl :: KTable) where
	-- |
	gDescribeTable :: proxy tbl -> Table

instance (KnownSymbol name, GColumns cols) => GTable ('TTable name cols) where
	gDescribeTable _ =
		Table (buildByteString (symbolVal (Proxy :: Proxy name)))
		      (gDescribeColumns (Proxy :: Proxy cols))

-- |
type family AnalyzeTableRep org (dat :: * -> *) :: KTable where
	-- Single constructor
	AnalyzeTableRep org (D1 meta1 (C1 ('MetaCons name f 'True) sel)) =
		'TTable name (AnalyzeRecordRep org sel)

	-- Data type with constructor(s) that does not match the given patterns
	AnalyzeTableRep org (D1 meta other) =
		TypeError ('Text "Given type "
		           ':<>: 'ShowType org
		           ':<>: 'Text " must have a single record constructor")

	-- Something else
	AnalyzeTableRep org other =
		TypeError ('Text "Given type "
		           ':<>: 'ShowType org
		           ':<>: 'Text " is not a valid data type"
		           ':$$: 'ShowType other)

-- |
type AnalyzeTable a = AnalyzeTableRep a (Rep a)

-- |
type GenericTable a = (Generic a, GTable (AnalyzeTable a))

-- |
describeGenericTable :: (GenericTable a) => proxy a -> Table
describeGenericTable proxy =
	gDescribeTable ((const Proxy :: proxy a -> Proxy (AnalyzeTable a)) proxy)

-- |
class TableEntity a where
	-- |
	describeTable :: proxy a -> Table

	default describeTable :: (GenericTable a) => proxy a -> Table
	describeTable = describeGenericTable

-- |
buildColumn :: Column -> QueryBuilder
buildColumn (Column name (ColumnType typeName notNull mbCheck)) = do
	insertName name
	insertCode " "
	insertName typeName

	when notNull (insertCode " NOT NULL")

	case mbCheck of
		Just gen -> do
			insertCode " "
			gen name

		Nothing -> pure ()

-- |
buildTableSchema :: Table -> QueryBuilder
buildTableSchema (Table name cols) =
	[pgsq| CREATE TABLE IF NOT EXISTS ${insertName name} ($colsDesc) |]
	where
		colsDesc = insertCommaSeperated (map buildColumn cols)
