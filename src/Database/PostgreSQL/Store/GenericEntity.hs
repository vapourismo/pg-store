{-# LANGUAGE ConstraintKinds,
             DataKinds,
             FlexibleContexts,
             FlexibleInstances,
             GADTs,
             StandaloneDeriving,
             TypeFamilies,
             TypeOperators,
             TypeSynonymInstances,
             UndecidableInstances
#-}

-- |
-- Module:     Database.PostgreSQL.Store.GenericEntity
-- Copyright:  (c) Ole Krüger 2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.GenericEntity (
	-- * Generic Entity
	EntityRep,
	GenericEntity,

	toGenericEntity,
	fromGenericEntity,


	-- * Type-Level Information
	KRecord (..),
	KFlatSum (..),
	KDataType (..),

	-- * Mapper classes
	CRecord (..),
	CFlatSum (..),
	CDataType (..),

	Record (..),
	FlatSum (..),
	DataType (..),

	-- * Analyzers
	AnalyzeRecordRep,
	AnalyzeFlatSumRep,
	AnalyzeDataType,

	AnalyzeEntity
) where

import GHC.Generics
import GHC.TypeLits

import Data.Kind

-- | Information about a record
data KRecord
	= TCombine KRecord KRecord
		-- ^ Combination of two records
	| TSingle Meta Type
		-- ^ Single element with meta information and type

-- | Mappings between a 'Generic' representation and our 'KRecord'-based representation
class CRecord (rec :: KRecord) where
	-- | 'Generic' representation
	type RecordRep rec :: * -> *

	-- | 'KRecord'-based representation
	data Record rec

	-- | From 'Generic' representation
	toRecord :: RecordRep rec x -> Record rec

	-- | To 'Generic' representation
	fromRecord :: Record rec -> RecordRep rec x

-- | Single record
instance CRecord ('TSingle meta typ) where
	type RecordRep ('TSingle meta typ) = S1 meta (Rec0 typ)

	data Record ('TSingle meta typ) = Single typ

	toRecord (M1 (K1 x)) = Single x

	fromRecord (Single x) = M1 (K1 x)

deriving instance (Show typ) => Show (Record ('TSingle meta typ))

-- | Combination of records
instance (CRecord lhs, CRecord rhs) => CRecord ('TCombine lhs rhs) where
	type RecordRep ('TCombine lhs rhs) = RecordRep lhs :*: RecordRep rhs

	data Record ('TCombine lhs rhs) = Combine (Record lhs) (Record rhs)

	toRecord (lhs :*: rhs) = Combine (toRecord lhs) (toRecord rhs)

	fromRecord (Combine lhs rhs) = fromRecord lhs :*: fromRecord rhs

deriving instance (Show (Record lhs), Show (Record rhs)) => Show (Record ('TCombine lhs rhs))

-- | Analyze the 'Generic' representation of the selectors. Make sure it has 1 or more fields. Then
-- transform it into a 'KRecord'.
type family AnalyzeRecordRep org (sel :: * -> *) :: KRecord where
	-- Single field
	AnalyzeRecordRep org (S1 meta (Rec0 typ)) =
		'TSingle meta typ

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

-- | Information about the constructors of an enumeration
data KFlatSum
	= TChoose KFlatSum KFlatSum
		-- ^ Combination of values
	| TValue Meta
		-- ^ Single value of the enumeration

-- | Mappings between a 'Generic' representation and our 'KFlatSum'-based representation
class CFlatSum (enum :: KFlatSum) where
	-- | 'Generic' representation
	type FlatSumRep enum :: * -> *

	-- | 'KFlatSum'-based representation
	data FlatSum enum

	-- | From 'Generic' representation
	toFlatSum :: FlatSumRep enum x -> FlatSum enum

	-- | To 'Generic' representation
	fromFlatSum :: FlatSum enum -> FlatSumRep enum x

-- | Single constructor
instance CFlatSum ('TValue meta) where
	type FlatSumRep ('TValue meta) = C1 meta U1

	data FlatSum ('TValue meta) = Unit

	toFlatSum (M1 U1) = Unit

	fromFlatSum Unit = M1 U1

deriving instance Show (FlatSum ('TValue meta))

-- | Combination of multiple constructors
instance (CFlatSum lhs, CFlatSum rhs) => CFlatSum ('TChoose lhs rhs) where
	type FlatSumRep ('TChoose lhs rhs) = FlatSumRep lhs :+: FlatSumRep rhs

	data FlatSum ('TChoose lhs rhs) = ChooseLeft (FlatSum lhs) | ChooseRight (FlatSum rhs)

	toFlatSum (L1 lhs) = ChooseLeft (toFlatSum lhs)
	toFlatSum (R1 rhs) = ChooseRight (toFlatSum rhs)

	fromFlatSum (ChooseLeft lhs)  = L1 (fromFlatSum lhs)
	fromFlatSum (ChooseRight rhs) = R1 (fromFlatSum rhs)

deriving instance (Show (FlatSum lhs), Show (FlatSum rhs)) => Show (FlatSum ('TChoose lhs rhs))

-- | Analyze the 'Generic' representation of constructors. Make sure every constructor has zero
-- fields. Then transform it into a 'KFlatSum'.
type family AnalyzeFlatSumRep org (cons :: * -> *) :: KFlatSum where
	-- Constructor without record selector
	AnalyzeFlatSumRep org (C1 meta U1) =
		'TValue meta

	-- Constructor with a record selector is invalid
	AnalyzeFlatSumRep org (C1 meta1 (S1 meta2 rec)) =
		TypeError ('Text "Given type "
		           ':<>: 'ShowType org
		           ':<>: 'Text " has multiple constructors, therefore these constructors must have \
		                       \no fields")

	-- More constructors
	AnalyzeFlatSumRep org (lhs :+: rhs) =
		'TChoose (AnalyzeFlatSumRep org lhs) (AnalyzeFlatSumRep org rhs)

	-- Something else
	AnalyzeFlatSumRep org other =
		TypeError ('Text "Given type "
		           ':<>: 'ShowType org
		           ':<>: 'Text " has an invalid constructor"
		           ':$$: 'ShowType other)

-- | Information about a data type
data KDataType
	= TRecord Meta Meta KRecord
		-- ^ Record
	| TFlatSum Meta KFlatSum
		-- ^ Enumeration

-- | Mappings between a 'Generic' representation and our 'KDataType'-based representation
class CDataType (dat :: KDataType) where
	-- | 'Generic' representation
	type GDataTypeRep dat :: * -> *

	-- | 'KDataType'-based representation
	data DataType dat

	-- | From 'Generic' representation
	toDataType :: GDataTypeRep dat x -> DataType dat

	-- | To 'Generic' representation
	fromDataType :: DataType dat -> GDataTypeRep dat x

-- | With single constructor
instance (CRecord rec) => CDataType ('TRecord d c rec) where
	type GDataTypeRep ('TRecord d c rec) = D1 d (C1 c (RecordRep rec))

	data DataType ('TRecord d c rec) = Record (Record rec)

	toDataType (M1 (M1 rec)) = Record (toRecord rec)

	fromDataType (Record rec) = M1 (M1 (fromRecord rec))

deriving instance (Show (Record rec)) => Show (DataType ('TRecord d c rec))

-- | With multiple constructors
instance (CFlatSum enum) => CDataType ('TFlatSum d enum) where
	type GDataTypeRep ('TFlatSum d enum) = D1 d (FlatSumRep enum)

	data DataType ('TFlatSum d enum) = FlatSum (FlatSum enum)

	toDataType (M1 enum) = FlatSum (toFlatSum enum)

	fromDataType (FlatSum flatSum) = M1 (fromFlatSum flatSum)

deriving instance (Show (FlatSum enum)) => Show (DataType ('TFlatSum d enum))

-- | Analyze the 'Generic' representation of a data type. If only one constructor exists, further
-- analyzing is delegated to 'AnalyzeRecordRep'. When two or more exist, analyzing is performed by
-- 'AnalyzeFlatSumRep'. The results are gather in a 'KDataType' instance.
type family AnalyzeDataType org (dat :: * -> *) :: KDataType where
	-- Single constructor
	AnalyzeDataType org (D1 meta1 (C1 meta2 sel)) =
		'TRecord meta1 meta2 (AnalyzeRecordRep org sel)

	-- Multiple constructors
	AnalyzeDataType org (D1 meta (lhs :+: rhs)) =
		'TFlatSum meta (AnalyzeFlatSumRep org (lhs :+: rhs))

	-- Missing constructor(s)
	AnalyzeDataType org (D1 meta V1) =
		TypeError ('Text "Given type "
		           ':<>: 'ShowType org
		           ':<>: 'Text " must have a constructor")

	-- Data type with constructor(s) that does not match the given patterns
	AnalyzeDataType org (D1 meta other) =
		TypeError ('Text "Given type "
		           ':<>: 'ShowType org
		           ':<>: 'Text " has an invalid constructor"
		           ':$$: 'ShowType other)

	-- Something else
	AnalyzeDataType org other =
		TypeError ('Text "Given type "
		           ':<>: 'ShowType org
		           ':<>: 'Text " is not a valid data type"
		           ':$$: 'ShowType other)

-- | Analyze the 'Generic' representation of a type, in order to generate its 'KDataType' instance.
type AnalyzeEntity a = AnalyzeDataType a (Rep a)

-- | Analyze the 'Generic' representation of a type to figure out which 'DataType' it needs.
type EntityRep a = DataType (AnalyzeEntity a)

-- | Make sure @a@ has a safe generic representation. Types that qualify implement 'Generic' and
-- fulfill one of the following criteria:
--
--  * single constructor with 1 or more fields
--  * multiple constructors with no fields
--
-- This constraint is mostly utilized to give the user more information about why their type has
-- been rejected.
type GenericEntity a = (Generic a,
                        CDataType (AnalyzeEntity a),
                        GDataTypeRep (AnalyzeEntity a) ~ Rep a)

-- | Convert to entity representation.
fromGenericEntity :: (GenericEntity a) => a -> EntityRep a
fromGenericEntity x = toDataType (from x)

-- | Build from entity representation.
toGenericEntity :: (GenericEntity a) => EntityRep a -> a
toGenericEntity x = to (fromDataType x)
