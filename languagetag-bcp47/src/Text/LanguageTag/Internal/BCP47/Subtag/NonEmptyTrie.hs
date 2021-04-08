{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Description : Internal subtag trie definitions
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- Warning\: the data constructors exported from this module are
-- unsafe to use: the values they take are expected by the rest of the
-- library to satisfy particular invariants that the type does not
-- enforce. Other components of the library may misbehave if
-- ill-formed values are given to them.
--
-- This module exports the non-empty 'Trie' types and basic functions
-- to create and manipulate them.
module Text.LanguageTag.Internal.BCP47.Subtag.NonEmptyTrie
  ( Trie (..),
    Node (..),
    Step (..),

    -- * Creation
    singleton,

    -- * Update and insertion
    insert,
    raise,
    modCreate,

    -- * Update and deletion
    delete,
    prune,
    prunePast,
    updateSub,
    alterSub,

    -- * Conversion
    fromTrie,
    fromStep,
  )
where

import Data.Bifunctor (first)
import Data.Functor.Classes
  ( Show1 (..),
    showsPrec1,
    showsUnaryWith,
  )
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Text.LanguageTag.Internal.BCP47.Subtag (Subtag)

-- | A strict, non-empty trie indexed by 'Subtag' values
data Trie a
  = -- | invariant: the 'Subtag' in the 'Step' must be less than all of the keys in the 'Map'
    Branch !(Node a) !(Step a) !(Map Subtag (Trie a))
  | Leaf !a
  deriving (Eq, Ord)

-- | Merge two 'Trie's together, also merging overlapping nodes
instance Semigroup a => Semigroup (Trie a) where
  Branch n (Step k t) m <> Branch n' (Step k' t') m' =
    case compare k k' of
      LT -> Branch (n <> n') (Step k t) $ modMerge m (M.insert k' t' m')
      EQ -> Branch (n <> n') (Step k $ t <> t') $ modMerge m m'
      GT -> Branch (n <> n') (Step k' t') $ modMerge (M.insert k t m) m'
    where
      modMerge = M.unionWith (<>)
  Branch n s m <> Leaf y = Branch (n <> Node y) s m
  Leaf x <> Branch n s m = Branch (Node x <> n) s m
  Leaf x <> Leaf y = Leaf $ x <> y

instance Functor Trie where
  fmap f (Branch n s m) = Branch (f <$> n) (f <$> s) (M.map (fmap f) m)
  fmap f (Leaf a) = Leaf $ f a

instance Foldable Trie where
  foldr f b (Branch n s m) = foldr f (foldr f (foldr (flip $ foldr f) b m) s) n
  foldr f b (Leaf a) = a `f` b
  null _ = False

-- | A node in a 'Trie'
data Node a
  = Node !a
  | Nil
  deriving (Eq, Ord, Functor, Foldable, Show)

-- | Lift the semigroup operation to possibly-absent nodes
instance Semigroup a => Semigroup (Node a) where
  Node x <> Node y = Node $ x <> y
  Node x <> Nil = Node x
  Nil <> Node y = Node y
  Nil <> Nil = Nil

instance Semigroup a => Monoid (Node a) where
  mempty = Nil

-- | A step in a 'Trie' path; a child of a 'Trie' node.
data Step a = Step !Subtag !(Trie a)
  deriving (Eq, Ord, Functor, Foldable, Show)

-- | Transform a 'Trie' into its constituent paths.
fromTrie :: Trie a -> NonEmpty ([Subtag], a)
fromTrie (Branch n (Step s t) m) = case n of
  Node x -> NE.cons ([], x) rest
  Nil -> rest
  where
    fixup (x, y) = first (x :) <$> y
    go x y = fixup $ fromStep $ Step x y
    (p :| ps) = go s t
    ms = concatMap (NE.toList . uncurry go) $ M.toList m
    rest = p :| ps <> ms
fromTrie (Leaf a) = ([], a) :| []

-- | Transform a 'Step' into its label and constituent paths.
fromStep :: Step a -> (Subtag, NonEmpty ([Subtag], a))
fromStep (Step s t) = (s, fromTrie t)

-- | Construct a 'Trie' with exactly the given node as content
singleton :: a -> [Subtag] -> Trie a
singleton = raise . Leaf

-- | Construct a 'Trie' with exactly the given sub-'Trie' at the end
-- of the path
raise :: Trie a -> [Subtag] -> Trie a
raise t (st : sts) = Branch Nil (Step st $ raise t sts) mempty
raise t [] = t

-- | Modify, create, or delete the sub-'Trie' at the given position,
-- returning 'Nothing' if this action would result in an empty 'Trie'
alterSub :: (Maybe (Trie a) -> Maybe (Trie a)) -> [Subtag] -> Trie a -> Maybe (Trie a)
alterSub f (st : sts) tr@(Branch n (Step k t) m) =
  case compare st k of
    LT -> case f Nothing of
      Just t' -> Just $ Branch n (Step st t') $ M.insert k t m
      Nothing -> Just tr
    EQ -> case alterSub f sts t of
      Nothing -> reform n m
      Just t' -> Just $ Branch n (Step k t') m
    GT -> Just $ Branch n (Step k t) $ M.update (alterSub f sts) st m
alterSub f (st : sts) (Leaf n) = case f Nothing of
  Just t -> Just $ Branch (Node n) (Step st $ raise t sts) mempty
  Nothing -> Just $ Leaf n
alterSub f [] tr = f $ Just tr

-- | Modify or delete the sub-'Trie' at the given position, returning
-- 'Nothing' if this action would result in an empty 'Trie'
updateSub :: (Trie a -> Maybe (Trie a)) -> [Subtag] -> Trie a -> Maybe (Trie a)
updateSub f (st : sts) tr@(Branch n (Step k t) m) =
  case compare st k of
    LT -> Just tr
    EQ -> case updateSub f sts t of
      Nothing -> reform n m
      Just t' -> Just $ Branch n (Step k t') m
    GT -> Just $ Branch n (Step k t) $ M.update (updateSub f sts) st m
updateSub _ (_ : _) tr@(Leaf _) = Just tr
updateSub f [] tr = f tr

-- | Modify or create the sub-'Trie' at the given position
modCreate :: (Maybe (Trie a) -> Trie a) -> [Subtag] -> Trie a -> Trie a
modCreate f (st : sts) (Branch n (Step k t) m) =
  case compare st k of
    LT -> Branch n (Step st $ raise (f Nothing) sts) $ M.insert k t m
    EQ -> Branch n (Step k $ modCreate f sts t) m
    GT -> Branch n (Step k t) $ M.alter go st m
  where
    go Nothing = Just $ raise (f Nothing) sts
    go (Just x) = Just $ modCreate f sts x
modCreate f (st : sts) (Leaf n) =
  Branch (Node n) (Step st $ raise (f Nothing) sts) mempty
modCreate f [] t = f $ Just t

-- | Replace the content of the given node of the 'Trie' with the
-- given value
insert :: a -> [Subtag] -> Trie a -> Trie a
insert a = modCreate go
  where
    go (Just (Branch _ s m)) = Branch (Node a) s m
    go _ = Leaf a

-- | Delete the content of the given node at the end, returning
-- 'Nothing' if that would result in an empty 'Trie'
delete :: [Subtag] -> Trie a -> Maybe (Trie a)
delete = updateSub go
  where
    go (Branch _ s m) = Just $ Branch Nil s m
    go (Leaf _) = Nothing

-- | Delete the node at the end of the given path and all of its
-- children, if it exists. Returns 'Nothing' if the resulting 'Trie'
-- would be empty.
prune :: [Subtag] -> Trie a -> Maybe (Trie a)
prune = updateSub $ const Nothing

-- | Delete the children of the given node, if it exists. Returns
-- 'Nothing' if the resulting 'Trie' would be empty.
prunePast :: [Subtag] -> Trie a -> Maybe (Trie a)
prunePast = updateSub go
  where
    go (Branch (Node a) _ _) = Just $ Leaf a
    go (Branch Nil _ _) = Nothing
    go (Leaf a) = Just $ Leaf a

-- | Attempt to turn a node and its children into a non-empty 'Trie'
reform :: Node a -> Map Subtag (Trie a) -> Maybe (Trie a)
reform n m = case M.toAscList m of
  ((k, v) : _) -> Just $ Branch n (Step k v) $ M.delete k m
  [] -> case n of
    Node a -> Just $ Leaf a
    Nil -> Nothing

instance Show a => Show (Trie a) where
  showsPrec = showsPrec1

instance Show1 Trie where
  liftShowsPrec sp sl d m =
    showsUnaryWith (liftShowsPrec sp' sl') "toTrie" d (fromTrie m)
    where
      sp' = liftShowsPrec sp sl
      sl' = liftShowList sp sl
