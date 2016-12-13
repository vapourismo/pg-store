{-# LANGUAGE OverloadedStrings,
             ConstraintKinds,
             DataKinds,
             FlexibleContexts,
             FlexibleInstances,
             TypeFamilies,
             TypeOperators,
             TypeSynonymInstances,
             UndecidableInstances,
             ScopedTypeVariables,
             QuasiQuotes,
             DefaultSignatures
#-}

-- |
-- Module:     Database.PostgreSQL.Store.Table
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

	expandColumns,
	expandColumnsOn,

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

import           Database.PostgreSQL.Store.Query.Builder
import           Database.PostgreSQL.Store.Utilities
import           Database.PostgreSQL.Store.ColumnEntity

-- | Type-level description of a record
data KColumns
	= TCombine KColumns KColumns
	| TSelector Symbol Type

-- | Desciption of a column
data Column = Column {
	-- | Column name
	colName :: B.ByteString,

	-- | Column type
	colType :: ColumnType
}

-- | Provide the means to demote 'KColumns' to a value.
class GColumns (rec :: KColumns) where
	-- | Instantiate singleton
	gDescribeColumns :: proxy rec -> [Column]

instance (KnownSymbol name, ColumnEntity typ) => GColumns ('TSelector name typ) where
	gDescribeColumns _ =
		[Column (buildByteString (symbolVal (Proxy :: Proxy name)))
		        (describeColumnType (Proxy :: Proxy typ))]

instance (GColumns lhs, GColumns rhs) => GColumns ('TCombine lhs rhs) where
	gDescribeColumns _ =
		gDescribeColumns (Proxy :: Proxy lhs)
		++ gDescribeColumns (Proxy :: Proxy rhs)

-- | Check the 'Generic' representation of a record in order to generate an instance of 'KColumns'.
type family AnalyzeRecordRep org (rec :: * -> *) :: KColumns where
	-- Single record field
	AnalyzeRecordRep org (S1 ('MetaSel ('Just name) m1 m2 m3) (Rec0 typ)) =
		'TSelector name typ

	-- Non-record field
	AnalyzeRecordRep org (S1 ('MetaSel 'Nothing m1 m2 m3) a) =
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

-- | Type-level description of a table
data KTable = TTable Symbol KColumns

-- | Description of a table
data Table = Table {
	-- | Table name
	tableName :: B.ByteString,

	-- | Table columns
	tableCols :: [Column]
}

-- | Provide the means to demote 'KTable' to a value.
class GTable (tbl :: KTable) where
	-- | Instantiate singleton
	gDescribeTable :: proxy tbl -> Table

instance (KnownSymbol name, GColumns cols) => GTable ('TTable name cols) where
	gDescribeTable _ =
		Table (buildByteString (symbolVal (Proxy :: Proxy name)))
		      (gDescribeColumns (Proxy :: Proxy cols))

-- | Check the 'Generic' representation of a data type in order to generate an instance of 'KTable'.
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

-- | Analyzes a type in order to retrieve its 'KTable' representation.
type AnalyzeTable a = AnalyzeTableRep a (Rep a)

-- | Constraint for generic tables
type GenericTable a = (Generic a, GTable (AnalyzeTable a))

-- | Fetch the table description for a generic table type.
describeGenericTable :: (GenericTable a) => proxy a -> Table
describeGenericTable proxy =
	gDescribeTable ((const Proxy :: proxy a -> Proxy (AnalyzeTable a)) proxy)

-- | Classify a type which can be used as a table.
class TableEntity a where
	-- | Describe the table type.
	describeTableType :: proxy a -> Table

	default describeTableType :: (GenericTable a) => proxy a -> Table
	describeTableType = describeGenericTable

-- | Build SQL code which describes the column.
buildColumn :: Column -> QueryBuilder
buildColumn (Column name (ColumnType typeName notNull mbCheck)) = do
	insertName name
	insertCode " "
	insertName typeName

	when notNull (insertCode " NOT NULL")

	case mbCheck of
		Just gen -> do
			insertCode " CHECK("
			gen name
			insertCode ")"

		Nothing -> pure ()

-- | Build the SQL code which describes and creates the table.
buildTableSchema :: Table -> QueryBuilder
buildTableSchema (Table name cols) = do
	insertCode "CREATE TABLE IF NOT EXISTS "
	insertName name
	insertCode "("
	insertCommaSeperated (map buildColumn cols)
	insertCode ")"

-- | Insert a comma-seperated list of the fully qualified column names of a table.
expandColumns :: Table -> QueryBuilder
expandColumns (Table name cols) =
	insertCommaSeperated (map insertColumn cols)
	where
		insertColumn (Column colName _) = do
			insertName name
			insertCode "."
			insertName colName

-- | Similar to 'expandColumns', but instead it expands the column names on an alias.
expandColumnsOn :: Table -> B.ByteString -> QueryBuilder
expandColumnsOn (Table _ cols) name =
	insertCommaSeperated (map insertColumn cols)
	where
		insertColumn (Column colName _) = do
			insertName name
			insertCode "."
			insertName colName
