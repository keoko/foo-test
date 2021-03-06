module Data.List.Lazy.Types where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Extend (class Extend)
import Control.Comonad (class Comonad)
import Control.Lazy as Z
import Control.MonadPlus (class MonadPlus)
import Control.MonadZero (class MonadZero)
import Control.Plus (class Plus)

import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.Lazy (Lazy, defer, force)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype, unwrap)
import Data.NonEmpty (NonEmpty, (:|))
import Data.NonEmpty as NE
import Data.Traversable (class Traversable, traverse, sequence)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable)

-- | A lazy linked list.
newtype List a = List (Lazy (Step a))

-- | A list is either empty (represented by the `Nil` constructor) or non-empty, in
-- | which case it consists of a head element, and another list (represented by the
-- | `Cons` constructor).
data Step a = Nil | Cons a (List a)

-- | Unwrap a lazy linked list
step :: forall a. List a -> Step a
step = force <<< unwrap

-- | The empty list.
-- |
-- | Running time: `O(1)`
nil :: forall a. List a
nil = List $ defer \_ -> Nil

-- | Attach an element to the front of a lazy list.
-- |
-- | Running time: `O(1)`
cons :: forall a. a -> List a -> List a
cons x xs = List $ defer \_ -> Cons x xs

-- | An infix alias for `cons`; attaches an element to the front of
-- | a list.
-- |
-- | Running time: `O(1)`
infixr 6 cons as :

derive instance newtypeList :: Newtype (List a) _

instance showList :: Show a => Show (List a) where
  show xs = "fromStrict (" <> go (step xs) <> ")"
    where
    go Nil = "Nil"
    go (Cons x xs) = "(Cons " <> show x <> " " <> go (step xs) <> ")"

instance eqList :: Eq a => Eq (List a) where
  eq xs ys = go (step xs) (step ys)
    where
    go Nil Nil = true
    go (Cons x xs) (Cons y ys)
      | x == y = go (step xs) (step ys)
    go _ _ = false

instance ordList :: Ord a => Ord (List a) where
  compare xs ys = go (step xs) (step ys)
    where
    go Nil Nil = EQ
    go Nil _   = LT
    go _   Nil = GT
    go (Cons x xs) (Cons y ys) =
      case compare x y of
        EQ -> go (step xs) (step ys)
        other -> other

instance lazyList :: Z.Lazy (List a) where
  defer f = List $ defer (step <<< f)

instance semigroupList :: Semigroup (List a) where
  append xs ys = List (go <$> unwrap xs)
    where
    go Nil = step ys
    go (Cons x xs) = Cons x (xs <> ys)

instance monoidList :: Monoid (List a) where
  mempty = nil

instance functorList :: Functor List where
  map f xs = List (go <$> unwrap xs)
    where
    go Nil = Nil
    go (Cons x xs) = Cons (f x) (f <$> xs)

instance foldableList :: Foldable List where
  foldr o b xs = go (step xs)
    where
    go Nil = b
    go (Cons a as) = a `o` foldr o b as

  foldl o b xs = go (step xs)
    where
    go Nil = b
    go (Cons a as) = foldl o (b `o` a) as

  foldMap f xs = go (step xs)
    where
    go Nil = mempty
    go (Cons x xs) = f x <> foldMap f xs

instance unfoldableList :: Unfoldable List where
  unfoldr f b = go (f b)
    where
    go Nothing = nil
    go (Just (Tuple a b)) = a : Z.defer \_ -> go (f b)

instance traversableList :: Traversable List where
  traverse f xs = go (step xs)
    where
    go Nil = pure nil
    go (Cons x xs) = cons <$> f x <*> traverse f xs

  sequence xs = go (step xs)
    where
    go Nil = pure nil
    go (Cons x xs) = cons <$> x <*> sequence xs

instance applyList :: Apply List where
  apply = ap

instance applicativeList :: Applicative List where
  pure = flip cons nil

instance bindList :: Bind List where
  bind xs f = List (go <$> unwrap xs)
    where
    go Nil = Nil
    go (Cons x xs) = step (f x <> bind xs f)

instance monadList :: Monad List

instance altList :: Alt List where
  alt = append

instance plusList :: Plus List where
  empty = nil

instance alternativeList :: Alternative List

instance monadZeroList :: MonadZero List

instance monadPlusList :: MonadPlus List

instance extendList :: Extend List where
  extend f l =
    case step l of
      Nil -> nil
      Cons a as ->
        f l : (foldr go { val: nil, acc: nil } as).val
        where
        go a { val, acc } =
          let acc' = a : acc
          in { val: f acc' : val, acc: acc' }

newtype NonEmptyList a = NonEmptyList (Lazy (NonEmpty List a))

toList :: forall a. NonEmptyList a -> List a
toList (NonEmptyList nel) = case force nel of x :| xs -> x : xs

derive instance newtypeNonEmptyList :: Newtype (NonEmptyList a) _

derive newtype instance eqNonEmptyList :: Eq a => Eq (NonEmptyList a)
derive newtype instance ordNonEmptyList :: Ord a => Ord (NonEmptyList a)

instance showNonEmptyList :: Show a => Show (NonEmptyList a) where
  show (NonEmptyList nel) = "(NonEmptyList " <> show nel <> ")"

instance functorNonEmptyList :: Functor NonEmptyList where
  map f (NonEmptyList nel) = NonEmptyList (map f <$> nel)

instance applyNonEmptyList :: Apply NonEmptyList where
  apply (NonEmptyList nefs) (NonEmptyList neas) =
    case force nefs, force neas of
      f :| fs, a :| as ->
        NonEmptyList (defer \_ -> f a :| (fs <*> a : nil) <> ((f : fs) <*> as))

instance applicativeNonEmptyList :: Applicative NonEmptyList where
  pure a = NonEmptyList (defer \_ -> NE.singleton a)

instance bindNonEmptyList :: Bind NonEmptyList where
  bind (NonEmptyList nel) f =
    case force nel of
      a :| as ->
        case force $ unwrap $ f a of
          b :| bs ->
            NonEmptyList (defer \_ -> b :| bs <> bind as (toList <<< f))

instance monadNonEmptyList :: Monad NonEmptyList

instance altNonEmptyList :: Alt NonEmptyList where
  alt = append

instance extendNonEmptyList :: Extend NonEmptyList where
  extend f w@(NonEmptyList nel) =
    case force nel of
      _ :| as ->
        NonEmptyList $ defer \_ ->
          f w :| (foldr go { val: nil, acc: nil } as).val
    where
    go a { val, acc } =
      { val: f (NonEmptyList (defer \_ -> a :| acc)) : val
      , acc: a : acc
      }

instance comonadNonEmptyList :: Comonad NonEmptyList where
  extract (NonEmptyList nel) = NE.head $ force nel

instance semigroupNonEmptyList :: Semigroup (NonEmptyList a) where
  append (NonEmptyList neas) as' =
    case force neas of
      a :| as -> NonEmptyList (defer \_ -> a :| as <> toList as')

instance foldableNonEmptyList :: Foldable NonEmptyList where
  foldr f b (NonEmptyList nel) = foldr f b (force nel)
  foldl f b (NonEmptyList nel) = foldl f b (force nel)
  foldMap f (NonEmptyList nel) = foldMap f (force nel)

instance traversableNonEmptyList :: Traversable NonEmptyList where
  traverse f (NonEmptyList nel) =
    map (\xxs -> NonEmptyList $ defer \_ -> xxs) $ traverse f (force nel)
  sequence (NonEmptyList nel) =
    map (\xxs -> NonEmptyList $ defer \_ -> xxs) $ sequence (force nel)
