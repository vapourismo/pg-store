{-# LANGUAGE ExistentialQuantification, UnboxedTuples #-}

module Database.PostgreSQL.Store.Query.Builder2 (
	QueryGenerator (..),
	withOther,
	assemble,
	assemblePrep
) where

import           Data.Semigroup

import qualified Data.ByteString as B

import           Database.PostgreSQL.Store.Types
import           Database.PostgreSQL.Store.Utilities

-- | Generator for queries, its type parameter hints the type needed to generate the attached values
data QueryGenerator a
	= Gen (a -> Value2)
	| Code B.ByteString
	| forall b. With (a -> b) (QueryGenerator b)
	| Merge (QueryGenerator a) (QueryGenerator a)

instance Monoid (QueryGenerator a) where
	mempty = Code B.empty

	mappend (Code l) (Code r) =
		Code (B.append l r)
	mappend (Code l) (Merge (Code r) suffix) =
		Merge (Code (B.append l r)) suffix
	mappend (Merge prefix (Code l)) (Code r) =
		Merge prefix (Code (B.append l r))
	mappend (Merge prefix (Code l)) (Merge (Code r) suffix) =
		Merge prefix (Merge (Code (B.append l r)) suffix)
	mappend lhs rhs =
		Merge lhs rhs

instance Semigroup (QueryGenerator a)

-- | Embed a generator which requires an external parameter.
withOther :: a -> QueryGenerator a -> QueryGenerator b
withOther x = With (const x)

-- | Assemble the query object.
assemble :: QueryGenerator a -> a -> Query2
assemble gen x =
	Query2 code values
	where
		(code, values, _) = walk gen x 1

		walk :: QueryGenerator b -> b -> Word -> (B.ByteString, [Value2], Word)
		walk gen x n =
			case gen of
				Gen f ->
					-- 36 = $
					(B.cons 36 (showByteString n), [f x], n + 1)

				Code c ->
					(c, [], n)

				Merge lhs rhs ->
					let
						(lc, lv, n')  = walk lhs x n
						(rc, rv, n'') = walk rhs x n'
					in (B.append lc rc, lv ++ rv, n'')

				With t gen' ->
					walk gen' (t x) n

-- | Assemble for query preparation.
assemblePrep :: B.ByteString -> QueryGenerator a -> PrepQuery2 a
assemblePrep name gen =
	PrepQuery2 name code values
	where
		(# code, values, _ #) = walk gen 1

		walk :: QueryGenerator b -> Word -> (# B.ByteString, b -> [Value2], Word #)
		walk gen n =
			case gen of
				Gen f ->
					(# B.cons 36 (showByteString n), \ x -> [f x], n + 1 #)

				Code c ->
					(# c, const [], n #)

				Merge lhs rhs ->
					let
						(# lc, lf, n' #)  = walk lhs n
						(# rc, rf, n'' #) = walk rhs n'
					in (# B.append lc rc, lf <> rf, n'' #)

				With t gen' ->
					let (# c, v, n' #) = walk gen' n in (# c, v . t, n' #)
