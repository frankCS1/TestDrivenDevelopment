-- BST.hs
-- Author: Frank
-- Description: Polymorphic binary search tree data structure for storing key-value pairs.
-- Developed using Test-Driven Development (TDD).
-- Skills applied from:
-- - Parametric types (Polymorphic Data Structures exercise)
-- - Functional recursion (List Processing & Luhn Algorithm exercises)
-- - Type signatures and guards (Grades.hs from earlier coursework)

module BST where

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
