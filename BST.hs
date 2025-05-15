module BST where

data BST k v
  = Empty
  | Node k v (BST k v) (BST k v)
  deriving (Show, Eq)
