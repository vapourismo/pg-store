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
	ApprovedGenericDataType,

	SafeGenericSel,
	SafeGenericEnumCons,
	SafeGenericCons,
	SafeGenericData
) where

import GHC.Generics
import GHC.TypeLits

class ApprovedGenericDataType

instance ApprovedGenericDataType

-- | Check the fields(s) that belong to the sole constructor of a data type.
type family SafeGenericSel org (sel :: * -> *) where
	-- No fields is invalid
	SafeGenericSel org U1 =
		TypeError ('Text "Given type "
		           ':<>: 'ShowType org
		           ':<>: 'Text " has one constructor, therefore that constructor must have \
		                       \at least one field")

	-- Single field
	SafeGenericSel org (S1 meta (Rec0 a)) =
		ApprovedGenericDataType

	-- Multiple fields
	SafeGenericSel org (lhs :*: rhs) =
		(SafeGenericSel org lhs, SafeGenericSel org rhs)

	-- Something else
	SafeGenericSel org other =
		TypeError ('Text "Given type "
		           ':<>: 'ShowType org
		           ':<>: 'Text " has a constructor with an invalid selector"
		           ':$$: 'ShowType other)

-- | Check the constructor(s) of a data type that has multiple constructors.
type family SafeGenericEnumCons org (cons :: * -> *) where
	-- Constructor without record selector
	SafeGenericEnumCons org (C1 meta U1) =
		ApprovedGenericDataType

	-- Constructor with a record selector is invalid
	SafeGenericEnumCons org (C1 meta1 (S1 meta2 rec)) =
		TypeError ('Text "Given type "
		           ':<>: 'ShowType org
		           ':<>: 'Text " has multiple constructors, therefore these constructors must have \
		                       \no fields")

	-- More constructors
	SafeGenericEnumCons org (lhs :+: rhs) =
		(SafeGenericEnumCons org lhs, SafeGenericEnumCons org rhs)

	-- Something else
	SafeGenericEnumCons org other =
		TypeError ('Text "Given type "
		           ':<>: 'ShowType org
		           ':<>: 'Text " has an invalid constructor"
		           ':$$: 'ShowType other)

-- | Check the constructor(s) of a data type.
type family SafeGenericCons org (cons :: * -> *) where
	-- No constructor
	SafeGenericCons org V1 =
		TypeError ('Text "Given type "
		           ':<>: 'ShowType org
		           ':<>: 'Text " must have a constructor")

	-- Single constructor
	SafeGenericCons org (C1 meta sel) =
		SafeGenericSel org sel

	-- Multiple constructors
	SafeGenericCons org (lhs :+: rhs) =
		(SafeGenericEnumCons org lhs, SafeGenericEnumCons org rhs)

	-- Something else
	SafeGenericCons org other =
		TypeError ('Text "Given type "
		           ':<>: 'ShowType org
		           ':<>: 'Text " has an invalid constructor"
		           ':$$: 'ShowType other)

-- | Check the type representation.
type family SafeGenericData org (dat :: * -> *) where
	-- Data type
	SafeGenericData org (D1 meta cons) =
		SafeGenericCons org cons

	-- Something else
	SafeGenericData org other =
		TypeError ('Text "Given type "
		           ':<>: 'ShowType org
		           ':<>: 'Text " is not a data type or does not implement the "
		           ':<>: 'ShowType Generic
		           ':<>: 'Text " type class"
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
