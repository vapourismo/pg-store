{-# LANGUAGE GADTs,
             TypeFamilies,
             TypeOperators,
             TypeApplications,
             ScopedTypeVariables,
             MultiParamTypeClasses,
             RankNTypes,
             DataKinds,
             PolyKinds,
             ConstraintKinds,
             FlexibleContexts,
             FlexibleInstances,
             UndecidableInstances,
             FunctionalDependencies,
             BangPatterns,
             StandaloneDeriving
#-}

module Database.PostgreSQL.Store.Tuple (
	Tuple (..),
	appendElement,

	getElementN,
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

	FunctionType,
	WithTuple,
	withTuple
) where

import GHC.TypeLits

import Data.List
import Data.Kind
import Data.Tagged

-- | Append a single element to the end of a list.
type family (|>) (x :: [a]) (y :: a) :: [a] where
	'[]       |> y = '[y]
	(x ': xs) |> y = x ': (xs |> y)

infixl 5 |>

-- | Generic product type
data Tuple (ts :: [Type]) where
	Empty :: Tuple '[]
	Cons  :: t -> !(Tuple ts) -> Tuple (t ': ts)

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
	getElementN :: Tuple ts -> Tagged n r

-- | Extract head element
instance HasElement 0 (t ': ts) t where
	getElementN (Cons x _) = Tagged x

	{-# INLINE getElementN #-}

-- | Extract element that is not the head
instance {-# OVERLAPPABLE #-} (1 <= n, HasElement (n - 1) ts r) => HasElement n (t ': ts) r where
	getElementN (Cons _ !xs) = retag (getElementN xs :: Tagged (n - 1) r)

	{-# INLINE getElementN #-}

-- | Extract element at index @0@.
getElement0 :: Tuple (r ': ts) -> r
getElement0 p = untag (getElementN @0 p)

-- | Extract element at index @1@.
getElement1 :: Tuple (t0 ': r ': ts) -> r
getElement1 p = untag (getElementN @1 p)

-- | Extract element at index @2@.
getElement2 :: Tuple (t0 ': t1 ': r ': ts) -> r
getElement2 p = untag (getElementN @2 p)

-- | Extract element at index @3@.
getElement3 :: Tuple (t0 ': t1 ': t2 ': r ': ts) -> r
getElement3 p = untag (getElementN @3 p)

-- | Extract element at index @4@.
getElement4 :: Tuple (t0 ': t1 ': t2 ': t3 ': r ': ts) -> r
getElement4 p = untag (getElementN @4 p)

-- | Extract element at index @5@.
getElement5 :: Tuple (t0 ': t1 ': t2 ': t3 ': t4 ': r ': ts) -> r
getElement5 p = untag (getElementN @5 p)

-- | Extract element at index @6@.
getElement6 :: Tuple (t0 ': t1 ': t2 ': t3 ': t4 ': t5 ': r ': ts) -> r
getElement6 p = untag (getElementN @6 p)

-- | Extract element at index @7@.
getElement7 :: Tuple (t0 ': t1 ': t2 ': t3 ': t4 ': t5 ': t6 ': r ': ts) -> r
getElement7 p = untag (getElementN @7 p)

-- | Extract element at index @8@.
getElement8 :: Tuple (t0 ': t1 ': t2 ': t3 ': t4 ': t5 ': t6 ': t7 ': r ': ts) -> r
getElement8 p = untag (getElementN @8 p)

-- | Extract element at index @9@.
getElement9 :: Tuple (t0 ': t1 ': t2 ': t3 ': t4 ': t5 ': t6 ': t7 ': t8 ': r ': ts) -> r
getElement9 p = untag (getElementN @9 p)

-- | Append an element to the end.
class AppendElement ts where
	appendElement :: Tuple ts -> t -> Tuple (ts |> t)

-- | Append to empty product.
instance AppendElement '[] where
	appendElement = flip Cons

	{-# INLINE appendElement #-}

-- | Append to non-empty product.
instance (AppendElement ts) => AppendElement (t ': ts) where
	appendElement (Cons y ys) x = Cons y (appendElement ys x)

	{-# INLINE appendElement #-}

-- | Do something with a 'Tuple'.
class ConsTuple ts a r | ts r -> a where
	consTuple :: Tuple ts -> a -> r

-- | Apply the given function to the current 'Tuple' state.
instance ConsTuple ts (Tuple ts -> r) r where
	consTuple state f = f state

-- | Collect and append product element, then continue.
instance (AppendElement ts, ConsTuple (ts |> t) a r) => ConsTuple ts a (t -> r) where
	consTuple state val x = consTuple (appendElement state x) val

-- | Build a function type using the given parameter types and return type.
type family FunctionType (ps :: [Type]) r where
	FunctionType '[]       r = r
	FunctionType (p ': ps) r = p -> FunctionType ps r

-- | A value of type @r@ can be created using an instance of @Tuple ts@.
type WithTuple ts r = ConsTuple '[] (Tuple ts -> r) (FunctionType ts r)

-- | Collect values to construct a @Tuple ts@, then apply the given function to it.
withTuple :: (WithTuple ts r) => (Tuple ts -> r) -> FunctionType ts r
withTuple = consTuple Empty
