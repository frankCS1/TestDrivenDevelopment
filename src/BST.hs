-- BST.hs
-- Author: Frank
-- Description: Polymorphic binary search tree data structure for storing key-value pairs.
-- Developed using Test-Driven Development (TDD).
-- Skills applied from:
-- - Parametric types (Polymorphic Data Structures exercise)
-- - Functional recursion (List Processing & Luhn Algorithm exercises)
-- - Type signatures and guards (Grades.hs from earlier coursework)

module BST (
    BST(..),        -- Exporting the data type so test cases can pattern match
    insert,         -- Insertion function
    lookupBST,       -- Lookup function
    countIf
) where

-- Count the number of values in the tree that satisfy a predicate.
-- Demonstrates use of higher-order functions and recursion.
-- Inspired by exercises on List Processing and Functional Patterns.
countIf :: (v -> Bool) -> BST k v -> Int
countIf _ Empty = 0
countIf pred (Node _ v left right) =
  let match = if pred v then 1 else 0
  in match + countIf pred left + countIf pred right

-- Define a generic Binary Search Tree (BST)
-- 'k' is the key type, which must be orderable (Ord)
-- 'v' is the value type
data BST k v
  = Empty
  | Node k v (BST k v) (BST k v)
  deriving (Show, Eq)  -- Enables tree display and equality comparisons (used in HUnit tests)

-- Insert a key-value pair into the BST.
-- Replaces value if the key already exists.
-- Developed using recursion and guards, building on pattern-matching skills from Grades.hs.
insert :: Ord k => k -> v -> BST k v -> BST k v
insert key val Empty = Node key val Empty Empty
insert key val (Node k v left right)
  | key < k   = Node k v (insert key val left) right
  | key > k   = Node k v left (insert key val right)
  | otherwise = Node key val left right  -- Replace existing value (no duplicate keys)

-- Lookup a value by key in the BST.
-- Returns Nothing if the key is not found.
-- Uses recursion and guards – developed using logic from List Processing and functional pattern matching.
lookupBST :: Ord k => k -> BST k v -> Maybe v
lookupBST _ Empty = Nothing
lookupBST key (Node k v left right)
  | key < k   = lookupBST key left
  | key > k   = lookupBST key right
  | otherwise = Just v
