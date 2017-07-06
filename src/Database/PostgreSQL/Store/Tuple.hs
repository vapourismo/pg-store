{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

-- |
-- Module:     Database.PostgreSQL.Store.Tuple
-- Copyright:  (c) Ole Krüger 2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.Tuple (
	Tuple (..),
	getElement,
	getElement0,
	getElement1,
	getElement2,
	getElement3,
	getElement4,
	getElement5,
	getElement6,
	getElement7,
	getElement8,
	getElement9,

	HasElement,

	Function,
	WithTuple,
	withTuple
) where

import GHC.TypeLits

import Data.Kind
import Data.List
import Data.Tagged

-- | Generic product type
data Tuple (ts :: [Type]) where
	Nil  :: Tuple '[]
	Cons :: t -> !(Tuple ts) -> Tuple (t ': ts)

-- | Helper class for the @Show (Tuple ts)@ instance
class ShowElement ts where
	gatherShown :: Tuple ts -> [String]

-- | Nothing to show
instance ShowElement '[] where
	gatherShown _ = []

-- | Show all elements, starting with the first
instance (Show t, ShowElement ts) => ShowElement (t ': ts) where
	gatherShown (Cons x rest) = show x : gatherShown rest

instance (ShowElement ts) => Show (Tuple ts) where
	show params = concat ["(", intercalate ", " (gatherShown params), ")"]

-- | Helper class to extract an element from a 'Tuple'.
class HasElement (n :: Nat) (ts :: [Type]) r | n ts -> r where
	-- | Extract the @n@-th element from the product.
	getElement :: Tuple ts -> Tagged n r

-- | Extract head element
instance HasElement 0 (t ': ts) t where
	getElement (Cons x _) = Tagged x

	{-# INLINE getElement #-}

-- | Extract element that is not the head
instance {-# OVERLAPPABLE #-} (1 <= n, HasElement (n - 1) ts r) => HasElement n (t ': ts) r where
	getElement (Cons _ !xs) = retag (getElement xs :: Tagged (n - 1) r)

	{-# INLINE getElement #-}

-- | Extract element at index @0@.
getElement0 :: (HasElement 0 ts r) => Tuple ts -> r
getElement0 p = untag (getElement @0 p)

-- | Extract element at index @1@.
getElement1 :: (HasElement 1 ts r) => Tuple ts -> r
getElement1 p = untag (getElement @1 p)

-- | Extract element at index @2@.
getElement2 :: (HasElement 2 ts r) => Tuple ts -> r
getElement2 p = untag (getElement @2 p)

-- | Extract element at index @3@.
getElement3 :: (HasElement 3 ts r) => Tuple ts -> r
getElement3 p = untag (getElement @3 p)

-- | Extract element at index @4@.
getElement4 :: (HasElement 4 ts r) => Tuple ts -> r
getElement4 p = untag (getElement @4 p)

-- | Extract element at index @5@.
getElement5 :: (HasElement 5 ts r) => Tuple ts -> r
getElement5 p = untag (getElement @5 p)

-- | Extract element at index @6@.
getElement6 :: (HasElement 6 ts r) => Tuple ts -> r
getElement6 p = untag (getElement @6 p)

-- | Extract element at index @7@.
getElement7 :: (HasElement 7 ts r) => Tuple ts -> r
getElement7 p = untag (getElement @7 p)

-- | Extract element at index @8@.
getElement8 :: (HasElement 8 ts r) => Tuple ts -> r
getElement8 p = untag (getElement @8 p)

-- | Extract element at index @9@.
getElement9 :: (HasElement 9 ts r) => Tuple ts -> r
getElement9 p = untag (getElement @9 p)
-- | Build a function type using the given parameter types and return type.
type family Function (ps :: [Type]) r where
	Function '[]      r = r
	Function (p : ps) r = p -> Function ps r

-- | Generate a function which collects the parameters and packs then into a 'Tuple.
class WithTuple (ts :: [Type]) where
	withTuple :: (Tuple ts -> r) -> Function ts r

instance WithTuple '[] where
	withTuple f = f Nil

instance (WithTuple ts) => WithTuple (t : ts) where
	withTuple f x = withTuple (\ xs -> f (Cons x xs))
