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
             BangPatterns
#-}

module Database.PostgreSQL.Store.Query.Parameters (
	Parameters (..),
	appendParam,
	extractParam,
	extractParam0,
	extractParam1,
	extractParam2,
	extractParam3,
	extractParam4,
	extractParam5,
	extractParam6,
	extractParam7,
	extractParam8,
	extractParam9,

	FunctionType,

	Constructible,
	constructWithParams
) where

import GHC.TypeLits

import Data.Kind
import Data.Tagged

-- | Append a single element to the end of the list.
type family (|>) (x :: [a]) (y :: a) :: [a] where
	'[]       |> y = '[y]
	(x ': xs) |> y = x ': (xs |> y)

infixl 5 |>

-- | Encapsules a list of types which are the parameters to something.
data Parameters (x :: [Type]) where
	Empty :: Parameters '[]
	Cons  :: t -> {-# UNPACK #-} !(Parameters ts) -> Parameters (t ': ts)

-- | An instance of 'Parameters' has a parameter of type @r@ at index @n@
class HasParam (n :: Nat) (ts :: [Type]) r | n ts -> r where
	-- | Extract the parameter.
	extractParam :: Parameters ts -> Tagged n r

-- | Parameter at index 0.
instance HasParam 0 (t ': ts) t where
	extractParam (Cons !x _) = Tagged x

	{-# INLINE extractParam #-}

-- | Parameter at an index greater or equal to 1.
instance {-# OVERLAPPABLE #-} (1 <= n, HasParam (n - 1) ts r) => HasParam n (t ': ts) r where
	extractParam (Cons _ !xs) =
		retag (extractParam xs :: Tagged (n - 1) r)

	{-# INLINE extractParam #-}

-- | Extract value at index @0@.
extractParam0 :: Parameters (r ': ts) -> r
extractParam0 p = untag (extractParam @0 p)

-- | Extract value at index @1@.
extractParam1 :: Parameters (t0 ': r ': ts) -> r
extractParam1 p = untag (extractParam @1 p)

-- | Extract value at index @2@.
extractParam2 :: Parameters (t0 ': t1 ': r ': ts) -> r
extractParam2 p = untag (extractParam @2 p)

-- | Extract value at index @3@.
extractParam3 :: Parameters (t0 ': t1 ': t2 ': r ': ts) -> r
extractParam3 p = untag (extractParam @3 p)

-- | Extract value at index @4@.
extractParam4 :: Parameters (t0 ': t1 ': t2 ': t3 ': r ': ts) -> r
extractParam4 p = untag (extractParam @4 p)

-- | Extract value at index @5@.
extractParam5 :: Parameters (t0 ': t1 ': t2 ': t3 ': t4 ': r ': ts) -> r
extractParam5 p = untag (extractParam @5 p)

-- | Extract value at index @6@.
extractParam6 :: Parameters (t0 ': t1 ': t2 ': t3 ': t4 ': t5 ': r ': ts) -> r
extractParam6 p = untag (extractParam @6 p)

-- | Extract value at index @7@.
extractParam7 :: Parameters (t0 ': t1 ': t2 ': t3 ': t4 ': t5 ': t6 ': r ': ts) -> r
extractParam7 p = untag (extractParam @7 p)

-- | Extract value at index @8@.
extractParam8 :: Parameters (t0 ': t1 ': t2 ': t3 ': t4 ': t5 ': t6 ': t7 ': r ': ts) -> r
extractParam8 p = untag (extractParam @8 p)

-- | Extract value at index @9@.
extractParam9 :: Parameters (t0 ': t1 ': t2 ': t3 ': t4 ': t5 ': t6 ': t7 ': t8 ': r ': ts) -> r
extractParam9 p = untag (extractParam @9 p)

-- | Append a parameter.
class AppendParam ts where
	appendParam :: t -> Parameters ts -> Parameters (ts |> t)

-- | To an empty list
instance AppendParam '[] where
	appendParam = Cons

	{-# INLINE appendParam #-}

-- | To a non-empty list
instance (AppendParam ts) => AppendParam (t ': ts) where
	appendParam x (Cons y ys) =
		Cons y (appendParam x ys)

	{-# INLINE appendParam #-}

-- | Build the parameters.
class BuildParams ts a r where
	buildParam :: Parameters ts -> a -> r

-- | Finalize the parameters.
instance BuildParams ts (Parameters ts -> r) r where
	buildParam state f = f state

-- | Capture parameters.
instance (AppendParam ts, BuildParams (ts |> t) a r) => BuildParams ts a (t -> r) where
	buildParam state val x =
		buildParam (appendParam x state) val

-- | Build the a function type using the given parameter types and return type.
type family FunctionType (ps :: [Type]) r where
	FunctionType '[]       r = r
	FunctionType (p ': ps) r = p -> FunctionType ps r

-- | An instance of @r@ can be created using @Parameters ts@.
type Constructible ts r = (BuildParams '[] (Parameters ts -> r) (FunctionType ts r))

-- | Transform the gathered parameters.
constructWithParams :: (Constructible ts r) => (Parameters ts -> r) -> FunctionType ts r
constructWithParams = buildParam Empty
