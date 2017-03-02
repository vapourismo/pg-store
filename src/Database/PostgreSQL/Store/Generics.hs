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
-- Module:     Database.PostgreSQL.Store.Generics
-- Copyright:  (c) Ole Krüger 2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.Generics (
	-- * Generic Entity
	Generic,
	Rep,

	toGeneric,
	fromGeneric,

	-- * Type-Level Information
	KRecord (..),
	KFlatSum (..),
	KDataType (..),

	-- * Mapper classes
	GRecord (..),
	GFlatSum (..),
	GDataType (..),

	Record (..),
	FlatSum (..),
	DataType (..),

	-- * Analyzers
	AnalyzeRecordRep,
	AnalyzeFlatSumRep,
	AnalyzeDataType
) where

import           GHC.Generics hiding (Generic (..))
import qualified GHC.Generics as G
import           GHC.TypeLits

import           Data.Kind

-- | Information about a record
data KRecord
	= TCombine KRecord KRecord
		-- ^ Combination of two records
	| TSingle Meta Type
		-- ^ Single element with meta information and type

-- | Mappings between a 'G.Generic' representation and our 'KRecord'-based representation
class GRecord (rec :: KRecord) where
	-- | 'Generic' representation
	type RecordRep rec :: * -> *

	-- | 'KRecord'-based representation
	data Record rec

	-- | From 'Generic' representation
	toRecord :: RecordRep rec x -> Record rec

	-- | To 'Generic' representation
	fromRecord :: Record rec -> RecordRep rec x

-- | Single record
instance GRecord ('TSingle meta typ) where
	type RecordRep ('TSingle meta typ) = S1 meta (Rec0 typ)

	data Record ('TSingle meta typ) = Single typ

	toRecord (M1 (K1 x)) = Single x

	fromRecord (Single x) = M1 (K1 x)

deriving instance (Show typ) => Show (Record ('TSingle meta typ))

-- | Combination of records
instance (GRecord lhs, GRecord rhs) => GRecord ('TCombine lhs rhs) where
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

-- | Mappings between a 'G.Generic' representation and our 'KFlatSum'-based representation
class GFlatSum (enum :: KFlatSum) where
	-- | 'Generic' representation
	type FlatSumRep enum :: * -> *

	-- | 'KFlatSum'-based representation
	data FlatSum enum

	-- | From 'Generic' representation
	toFlatSum :: FlatSumRep enum x -> FlatSum enum

	-- | To 'Generic' representation
	fromFlatSum :: FlatSum enum -> FlatSumRep enum x

-- | Single constructor
instance GFlatSum ('TValue meta) where
	type FlatSumRep ('TValue meta) = C1 meta U1

	data FlatSum ('TValue meta) = Unit

	toFlatSum (M1 U1) = Unit

	fromFlatSum Unit = M1 U1

deriving instance Show (FlatSum ('TValue meta))

-- | Combination of multiple constructors
instance (GFlatSum lhs, GFlatSum rhs) => GFlatSum ('TChoose lhs rhs) where
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

	-- Constructor with a record selector is invalid
	AnalyzeFlatSumRep org (C1 meta1 (lhs :*: rhs)) =
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

-- | Mappings between a 'G.Generic' representation and our 'KDataType'-based representation
class GDataType (dat :: KDataType) where
	-- | 'Generic' representation
	type DataTypeRep dat :: * -> *

	-- | 'KDataType'-based representation
	data DataType dat

	-- | From 'Generic' representation
	toDataType :: DataTypeRep dat x -> DataType dat

	-- | To 'Generic' representation
	fromDataType :: DataType dat -> DataTypeRep dat x

-- | With single constructor
instance (GRecord rec) => GDataType ('TRecord d c rec) where
	type DataTypeRep ('TRecord d c rec) = D1 d (C1 c (RecordRep rec))

	data DataType ('TRecord d c rec) = Record (Record rec)

	toDataType (M1 (M1 rec)) = Record (toRecord rec)

	fromDataType (Record rec) = M1 (M1 (fromRecord rec))

deriving instance (Show (Record rec)) => Show (DataType ('TRecord d c rec))

-- | With multiple constructors
instance (GFlatSum enum) => GDataType ('TFlatSum d enum) where
	type DataTypeRep ('TFlatSum d enum) = D1 d (FlatSumRep enum)

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

-- | 'KDataType' representation of a data type
type Rep a = AnalyzeDataType a (G.Rep a)

-- | Make sure @a@ has a safe generic representation. Types that qualify implement 'G.Generic' (GHC)
-- and fulfill one of the following criteria:
--
--  * single constructor with 1 or more fields
--  * multiple constructors with no fields
--
-- This constraint is mostly utilized to give the user more information about why their type has
-- been rejected.
type Generic a = (G.Generic a, GDataType (Rep a), DataTypeRep (Rep a) ~ G.Rep a)

-- | Convert to generic representation.
fromGeneric :: (Generic a) => a -> DataType (Rep a)
fromGeneric = toDataType . G.from

-- | Build from generic representation.
toGeneric :: (Generic a) => DataType (Rep a) -> a
toGeneric = G.to . fromDataType
