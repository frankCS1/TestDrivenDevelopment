-- BST.hs
-- Author: Frank
-- Defines a parametric binary search tree (BST) for dictionary-style key-value storage.
-- This structure will be used as the basis for implementing a functional dictionary.

module BST where

-- The BST data type is parametrically polymorphic in both key (k) and value (v).
-- Each Node contains a key, associated value, and left/right subtrees.
-- Empty represents an empty tree (base case for recursion).

data BST k v
  = Empty
  | Node k v (BST k v) (BST k v)
  deriving (Show, Eq)  -- Deriving Show for debugging; Eq for equality-based testing
