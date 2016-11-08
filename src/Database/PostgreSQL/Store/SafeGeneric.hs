{-# LANGUAGE DataKinds, ScopedTypeVariables, KindSignatures, TypeSynonymInstances,
             FlexibleInstances, TypeFamilies, TypeOperators, FlexibleContexts, UndecidableInstances,
             ConstraintKinds, MultiParamTypeClasses, PolyKinds #-}

-- |
-- Module:     Database.PostgreSQL.Store.SafeGeneric
-- Copyright:  (c) Ole Krüger 2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.SafeGeneric (
	-- * Safe Generic
	SafeGeneric,

	-- * Helpers
	SafeGenericRecord,
	SafeGenericEnum,
	SafeGenericData
) where

import GHC.Generics
import GHC.TypeLits

class ApprovedGenericDataType

instance ApprovedGenericDataType

-- | Check the fields(s) that belong to the sole constructor of a data type.
type family SafeGenericRecord org (sel :: * -> *) where
	-- Single field
	SafeGenericRecord org (S1 meta (Rec0 a)) =
		ApprovedGenericDataType

	-- Multiple fields
	SafeGenericRecord org (lhs :*: rhs) =
		(SafeGenericRecord org lhs, SafeGenericRecord org rhs)

	-- Missing field(s)
	SafeGenericRecord org U1 =
		TypeError ('Text "Given type "
		           ':<>: 'ShowType org
		           ':<>: 'Text " has one constructor, therefore that constructor must have \
		                       \at least one field")

	-- Something else
	SafeGenericRecord org other =
		TypeError ('Text "Given type "
		           ':<>: 'ShowType org
		           ':<>: 'Text " has a constructor with an invalid selector"
		           ':$$: 'ShowType other)

-- | Check the constructor(s) of a data type that has multiple constructors.
type family SafeGenericEnum org (cons :: * -> *) where
	-- Constructor without record selector
	SafeGenericEnum org (C1 meta U1) =
		ApprovedGenericDataType

	-- Constructor with a record selector is invalid
	SafeGenericEnum org (C1 meta1 (S1 meta2 rec)) =
		TypeError ('Text "Given type "
		           ':<>: 'ShowType org
		           ':<>: 'Text " has multiple constructors, therefore these constructors must have \
		                       \no fields")

	-- More constructors
	SafeGenericEnum org (lhs :+: rhs) =
		(SafeGenericEnum org lhs, SafeGenericEnum org rhs)

	-- Something else
	SafeGenericEnum org other =
		TypeError ('Text "Given type "
		           ':<>: 'ShowType org
		           ':<>: 'Text " has an invalid constructor"
		           ':$$: 'ShowType other)

-- | Check the type representation.
type family SafeGenericData org (dat :: * -> *) where
	-- Single constructor
	SafeGenericData org (D1 meta1 (C1 meta2 sel)) =
		SafeGenericRecord org sel

	-- Multiple constructors
	SafeGenericData org (D1 meta (lhs :+: rhs)) =
		(SafeGenericEnum org lhs, SafeGenericEnum org rhs)

	-- Missing constructor(s)
	SafeGenericData org (D1 meta V1) =
		TypeError ('Text "Given type "
		           ':<>: 'ShowType org
		           ':<>: 'Text " must have a constructor")

	-- Data type with constructor(s) that does not match the given patterns
	SafeGenericData org (D1 meta other) =
		TypeError ('Text "Given type "
		           ':<>: 'ShowType org
		           ':<>: 'Text " has an invalid constructor"
		           ':$$: 'ShowType other)

	-- Something else
	SafeGenericData org other =
		TypeError ('Text "Given type "
		           ':<>: 'ShowType org
		           ':<>: 'Text " is not a valid data type"
		           ':$$: 'ShowType other)

-- | Make sure the given type @a@ and matches one of the following criteria:
--
--  * single constructor with 1 or more records
--  * multiple constructors with no records
--
-- Also the constraints @Generic a@ and @con (Rep a)@ must hold true.
--
-- This constraint is mostly utilized to give the user more information about why their type has
-- been rejected.
type SafeGeneric con a = (Generic a, SafeGenericData a (Rep a), con (Rep a))
