{-# LANGUAGE GADTs #-}
{- | An Unique List module

  This is like a list, so an ordered container on which we can snoc
  element and get them by index
  However there is a unicity of the inserted element. The list is
  left biased, in case of duplicate, the left element is stored,
  when the right one is discarded, see `snoc` and `replace`.

  This module is totally useless, don't read it. It was created as an
  example for a friend.

  TV: You can read the comment, I try to explain some Haskell subtetly.
-}
module UniqueList
  (
    -- Encapsulation is done by not exporting some function
    -- But you can export everything if you like
    UniqueList, -- Internal is not exposed
    empty,
    size,
    singleton,
    fromList,
    snoc,
    concat,
    member,
    replace,
    idx
  )

where

import qualified Data.Set as Set
import Data.Monoid ((<>))
import qualified Data.Sequence as Seq

-- | The UniqueList type. Polymorphic on the contained type `t`.
data UniqueList t where
  UniqueList :: (Seq.Seq t) -> Set.Set t -> UniqueList t
-- The previous line is important, because it defines :
-- - The type `UniqueList`, which is polymorphic on the type of the contained item.
-- - The constructor `UniqueList`, which takes a list of t, a set of t and returns an UniqueList t
--   Think about it as a struct of two field, a list and a set
-- However this is important because there is a bidirectional mapping between constructor and pattern matching as you'll see later

-- NOTE: all the type annotations (the stuff with `::` are not needed, but it helps to read
-- NOTE: coments with `|` are a special markup for documentation, ala docstring.

-- | Empty one. O(1)
empty :: UniqueList t
empty = UniqueList (Seq.empty) (Set.empty)
-- this uses the constructor to set an empty list and an empty set

-- | Create a singleton. O(1).
singleton :: t -> UniqueList t
singleton v = UniqueList (Seq.singleton v) (Set.singleton v)
-- See how the constructor is used to create a singleton list and a singleton set

-- | Returns the size. O(1).
size :: UniqueList t -> Int
size (UniqueList l _) = Seq.length l -- sequences have O(1) size
-- See here how the constructor is used on the left side of an `=`
-- operator, which means that it is used as a pattern match, or un
-- unpack operator. Here, the inner list will be stored inside `_` (so
-- ignored) and the inner set will be stored in `s`.

-- | Append an element. O(log n).
--   If the element is already present, the returned list is unmodified.
snoc :: Ord t => t -> UniqueList t -> UniqueList t
snoc i ul@(UniqueList l s)
  | i `Set.member` s = ul -- Don't do anything if it is in the set
  | otherwise = UniqueList (l Seq.|> i) (Set.insert i s) -- cons it to the list and insert it in the set otherwise
-- The a@Pattern syntax means that `ul` will be the unique list, which
-- will alse be unpacked to l and s. That's just syntax convenience.

-- | Convert from a foldable. O(n log n).
fromList :: (Foldable f, Ord t) => f t -> UniqueList t
fromList l = foldl (flip snoc) empty l
-- Apply a fold on the input `Foldable`. A `Foldable` is a generalisation of an iterable.
-- this roughly means that fromList [1,2,3] = snoc 3 (snoc 2 (snoc 1 empty))

-- | `member v l` returns true if `l` contains `v`. O(log n).
member :: Ord t => t -> UniqueList t -> Bool
member v (UniqueList _ s) = v `Set.member` s
-- simple Set test.
-- NOTE: in haskell, function can be applied prefix or infix, both syntax are equal :
--  - Set.member v s
--  - v `Set.member` s
-- depending on the context, one or the other can enhance readability

-- | Replace an element. O(n log n).
--   This is the same as `snoc` except that the list keeps the last inserted item.
replace :: Ord t => t -> UniqueList t -> UniqueList t
replace i ul@(UniqueList l s)
  | i `Set.notMember` s = snoc i ul -- if the item is not a member of the set, just snoc it
  | otherwise = let l' = ((Seq.filter (/=i) l) Seq.|> i) -- otherwise, we build a new inner list by prepending the new value to the previous list where the previous value was filtered...
                in UniqueList l' s

-- | Return the element at index. O(n) if present, O(1) else.
idx :: Int -> UniqueList t -> Maybe t
idx offset (UniqueList l _)
  | offset < Seq.length l = Just (l `Seq.index` offset)
  | otherwise = Nothing

-- Here starts some magic when you don't know Haskell, but actually that's pretty common
-- That's instance of different classes. That's the way of overloading functions based on type.

-- The show function will display a nice string for our type
instance Show t => Show (UniqueList t) where
  show (UniqueList t _) = "fromList " ++ show t

-- The Foldable instance means we can use a shitload of function on iterable with just the definition of `foldMap`.
-- I'm using the foldMap instance on the inner list
-- This gives me fold, and many useful function, such as foldMap, sum, maximum, minimum, ...
instance Foldable UniqueList where
  foldMap f (UniqueList l _) = foldMap f l

-- I also implemented Monoid, because concatenation of two list is a monoid
instance Ord t => Monoid (UniqueList t) where
  mempty = empty
  -- | Concat two unique lists. Still biased toward first elements. O(n log n)
  mappend (UniqueList la _) (UniqueList lb _) = fromList (la <> lb)
