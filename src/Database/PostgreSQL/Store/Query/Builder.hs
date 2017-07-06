{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeOperators             #-}

-- |
-- Module:     Database.PostgreSQL.Store.Query.Builder
-- Copyright:  (c) Ole Krüger 2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.Query.Builder (
	QueryGenerator (..),

	assemble,
	assemblePrep,

	withOther,

	genIdentifier,
	genNestedIdentifier,
	genQuote,

	joinGens,

	withParamN,
	withParam0,
	withParam1,
	withParam2,
	withParam3,
	withParam4,
	withParam5,
	withParam6,
	withParam7,
	withParam8,
	withParam9
) where

import qualified Data.ByteString                     as B
import           Data.Hashable
import           Data.List
import           Data.Semigroup
import           Data.String
import           Data.Tagged

import           Database.PostgreSQL.Store.Tuple
import           Database.PostgreSQL.Store.Types
import           Database.PostgreSQL.Store.Utilities

-- | Generator for queries, its type parameter hints the type needed to generate the attached values
data QueryGenerator a
	= Gen Oid (a -> Maybe B.ByteString)
	| Code B.ByteString
	| forall b. With (a -> b) (QueryGenerator b)
	| Merge (QueryGenerator a) (QueryGenerator a)

instance Monoid (QueryGenerator a) where
	mempty = Code B.empty

	mappend (Code l) (Code r) =
		Code (B.append l r)
	mappend (Code l) (Merge (Code r) suffix) =
		mappend (Code (B.append l r)) suffix
	mappend (Merge prefix (Code l)) (Code r) =
		mappend prefix (Code (B.append l r))
	mappend (Merge prefix (Code l)) (Merge (Code r) suffix) =
		mappend prefix (Merge (Code (B.append l r)) suffix)
	mappend lhs rhs =
		Merge lhs rhs

	{-# INLINE mappend #-}

instance Semigroup (QueryGenerator a)

instance IsString (QueryGenerator a) where
	fromString str = Code (buildByteString str)

instance Hashable (QueryGenerator a) where
	hashWithSalt salt (Gen _ _)   = hashWithSalt salt ()
	hashWithSalt salt (Code c)    = hashWithSalt salt c
	hashWithSalt salt (With _ c)  = hashWithSalt salt c
	hashWithSalt salt (Merge l r) = hashWithSalt (hashWithSalt salt l) r

-- | Assemble the query object.
assemble :: QueryGenerator a -> a -> Query r
assemble gen x =
	Query code values
	where
		(code, values, _) = walk gen x 1

		walk :: QueryGenerator b -> b -> Word -> (B.ByteString, [Maybe (Oid, B.ByteString, Format)], Word)
		walk gen x n =
			case gen of
				Gen typ f ->
					-- 36 = $
					(B.cons 36 (showByteString n), [toTypedParam typ <$> f x], n + 1)

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
assemblePrep :: B.ByteString -> QueryGenerator (Tuple p) -> PrepQuery p r
assemblePrep prefix gen =
	PrepQuery (B.append prefix (showByteString (hash code))) code oids values
	where
		(code, oids, values, _) = walk gen 1

		walk :: QueryGenerator b -> Word -> (B.ByteString, [Oid], b -> [Maybe (B.ByteString, Format)], Word)
		walk gen n =
			case gen of
				Gen typ f ->
					(B.cons 36 (showByteString n), [typ], \ x -> [toParam <$> f x], n + 1)

				Code c ->
					(c, [], const [], n)

				Merge lhs rhs ->
					let
						(lc, lt, lf, n')  = walk lhs n
						(rc, rt, rf, n'') = walk rhs n'
					in (B.append lc rc, lt ++ rt, lf <> rf, n'')

				With f gen' ->
					let (c, t, v, n') = walk gen' n in (c, t, v . f, n')

-- | Embed a generator which requires an external parameter.
withOther :: a -> QueryGenerator a -> QueryGenerator b
withOther x = With (const x)

-- | Format identifier properly.
formatIdentifier :: B.ByteString -> B.ByteString
formatIdentifier name =
	if isAllowed then
		name
	else
		B.concat [B.singleton 34, -- "
		          B.intercalate (B.pack [34, 34]) (B.split 34 name),
		          B.singleton 34]
	where
		isAllowedHead b =
			(b >= 97 && b <= 122)    -- 'a' to 'z'
			|| (b >= 65 && b <= 90)  -- 'A' to 'Z'
			|| b == 95               -- '_'

		isAllowedBody b =
			isAllowedHead b
			|| (b >= 48 && b <= 57)  -- '0' to '9'

		isAllowed =
			case B.uncons name of
				Nothing     -> False
				Just (h, b) -> isAllowedHead h && B.all isAllowedBody b

-- | Insert an identifying name. Takes care of proper quotation.
genIdentifier :: B.ByteString -> QueryGenerator a
genIdentifier name =
	Code (formatIdentifier name)

-- | Connect two identifiers with a dot. Each identifier is surrounded by quotes if necessary.
genNestedIdentifier :: B.ByteString -> B.ByteString -> QueryGenerator a
genNestedIdentifier target field =
	Code (B.concat [formatIdentifier target,
	                B.singleton 46, -- .
	                formatIdentifier field])

-- | Surround with quotes and escape delimiting characters.
genQuote :: B.ByteString -> QueryGenerator a
genQuote contents =
	Code (B.concat [B.singleton 39, -- '
	                B.intercalate (B.pack [39, 39]) (B.split 39 contents),
	                B.singleton 39])

-- | Join multiple query generators with a piece of code.
joinGens :: B.ByteString -> [QueryGenerator a] -> QueryGenerator a
joinGens code gens =
	mconcat (intersperse (Code code) gens)

-- | Redirect the @n@-th parameter for the given query
withParamN :: forall n r ts. (HasElement n ts r)
           => QueryGenerator r
           -> Tagged n (QueryGenerator (Tuple ts))
withParamN x = Tagged (With (untag . getElement @n) x)

-- | Redirect the 0th paramter to the given query generator.
withParam0 :: QueryGenerator r -> QueryGenerator (Tuple (r ': ts))
withParam0 = With getElement0

-- | Redirect the 1st paramter to the given query generator.
withParam1 :: QueryGenerator r -> QueryGenerator (Tuple (t0 ': r ': ts))
withParam1 = With getElement1

-- | Redirect the 2nd paramter to the given query generator.
withParam2 :: QueryGenerator r -> QueryGenerator (Tuple (t0 ': t1 ': r ': ts))
withParam2 = With getElement2

-- | Redirect the 3rd paramter to the given query generator.
withParam3 :: QueryGenerator r -> QueryGenerator (Tuple (t0 ': t1 ': t2 ': r ': ts))
withParam3 = With getElement3

-- | Redirect the 4th paramter to the given query generator.
withParam4 :: QueryGenerator r -> QueryGenerator (Tuple (t0 ': t1 ': t2 ': t3 ': r ': ts))
withParam4 = With getElement4

-- | Redirect the 5th paramter to the given query generator.
withParam5 :: QueryGenerator r -> QueryGenerator (Tuple (t0 ': t1 ': t2 ': t3 ': t4 ': r ': ts))
withParam5 = With getElement5

-- | Redirect the 6th paramter to the given query generator.
withParam6 :: QueryGenerator r -> QueryGenerator (Tuple (t0 ': t1 ': t2 ': t3 ': t4 ': t5 ': r ': ts))
withParam6 = With getElement6

-- | Redirect the 7th paramter to the given query generator.
withParam7 :: QueryGenerator r -> QueryGenerator (Tuple (t0 ': t1 ': t2 ': t3 ': t4 ': t5 ': t6 ': r ': ts))
withParam7 = With getElement7

-- | Redirect the 8th paramter to the given query generator.
withParam8 :: QueryGenerator r -> QueryGenerator (Tuple (t0 ': t1 ': t2 ': t3 ': t4 ': t5 ': t6 ': t7 ': r ': ts))
withParam8 = With getElement8

-- | Redirect the 9th paramter to the given query generator.
withParam9 :: QueryGenerator r -> QueryGenerator (Tuple (t0 ': t1 ': t2 ': t3 ': t4 ': t5 ': t6 ': t7 ': t8 ': r ': ts))
withParam9 = With getElement9
