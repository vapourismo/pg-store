{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module:     Database.PostgreSQL.Store.Table
-- Copyright:  (c) Ole Krüger 2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.Table (
	-- * Table
	Table (..),
	TableEntity (..),

	genTableName,
	genTableColumns,
	genTableColumnsOn,

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

import qualified Data.ByteString                         as B
import           Data.Kind
import           Data.Proxy
import           Data.Tagged

import           Database.PostgreSQL.Store.Entity
import           Database.PostgreSQL.Store.Query.Builder
import           Database.PostgreSQL.Store.Utilities

-- | Type-level description of a record
data KColumns
	= TCombine KColumns KColumns
	| TSelector Symbol Type

-- | Provide the means to demote 'KColumns' to a value.
class GColumns (rec :: KColumns) where
	-- | Instantiate singleton
	gDescribeColumns :: Tagged rec [B.ByteString]

instance (KnownSymbol name) => GColumns ('TSelector name typ) where
	gDescribeColumns =
		Tagged [buildByteString (symbolVal @name Proxy)]

instance (GColumns lhs, GColumns rhs) => GColumns ('TCombine lhs rhs) where
	gDescribeColumns =
		Tagged (untag (gDescribeColumns @lhs) ++ untag (gDescribeColumns @rhs))

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
	tableCols :: [B.ByteString]
} deriving (Show, Eq, Ord)

-- | Provide the means to demote 'KTable' to a value.
class GTable (tbl :: KTable) where
	-- | Instantiate singleton
	gDescribeTable :: Tagged tbl Table

instance (KnownSymbol name, GColumns cols) => GTable ('TTable name cols) where
	gDescribeTable =
		Tagged (Table (buildByteString (symbolVal @name Proxy))
		              (untag (gDescribeColumns @cols)))

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
describeGenericTable :: forall a. (GenericTable a) => Tagged a Table
describeGenericTable =
	retag (gDescribeTable @(AnalyzeTable a))

-- | Table entity with extra information about its name and column names
class (Entity a) => TableEntity a where
	-- | Describe the table type.
	describeTableType :: Tagged a Table

	default describeTableType :: (GenericTable a) => Tagged a Table
	describeTableType = describeGenericTable

-- | Embed table name.
genTableName :: Table -> QueryGenerator a
genTableName (Table name _) =
	genIdentifier name

-- | Embed a comma-seperated list of the table's columns.
genTableColumns :: Table -> QueryGenerator a
genTableColumns (Table name columns) =
	joinGens (B.singleton 44) (map (genNestedIdentifier name) columns)

-- | Same as 'genTableColumns' but expands the columns on an alias of the table name.
genTableColumnsOn :: Table -> B.ByteString -> QueryGenerator a
genTableColumnsOn (Table _ columns) name =
	joinGens (B.singleton 44) (map (genNestedIdentifier name) columns)
