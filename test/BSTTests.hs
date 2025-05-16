-- BSTTests.hs
-- Author: Frank
-- Description: HUnit-based unit tests for the BST module.
-- Developed using TDD, following skills from:
-- - HUnit Testing exercise (Automated HUnit Testing.html)
-- - Functional test design from Grades.hs
-- - Test suite structure and assertions

module Main where

import Test.HUnit      -- Imported from HUnit exercise
import BST             -- Our module under test (BST.hs)

-- Test that the Empty constructor behaves as expected.
-- This is a basic test confirming BST data type structure.
testEmpty :: Test
testEmpty = TestCase (
  let tree :: BST Int String
      tree = Empty
  in assertEqual "Empty tree should equal Empty" tree tree
  )

-- Test inserting a single key-value pair into an empty BST.
-- This test was written before the insert function existed,
-- demonstrating the first step in the TDD cycle.
testInsertSingle :: Test
testInsertSingle =
  let result = insert 5 "five" Empty
      expected = Node 5 "five" Empty Empty
  in TestCase (assertEqual "Insert single node into empty BST" expected result)

-- Test inserting multiple nodes to ensure correct placement in left and right subtrees.
-- Reinforces recursion and ordering from Grades.hs and Polymorphic Structures exercises.
testInsertMultiple :: Test
testInsertMultiple =
  let result = insert 7 "seven" (insert 3 "three" (insert 5 "five" Empty))
      expected = Node 5 "five"
                    (Node 3 "three" Empty Empty)
                    (Node 7 "seven" Empty Empty)
  in TestCase (assertEqual "Insert left and right nodes into BST" expected result)

-- Test lookup on a BST containing one key-value pair.
-- Skills applied: pattern matching, Maybe type handling (from List Processing + Grades.hs logic).
testLookupSingle :: Test
testLookupSingle =
  let tree = insert 5 "five" Empty
      result = lookupBST 5 tree
  in TestCase (assertEqual "Lookup existing key in single-node tree" (Just "five") result)

-- Main test runner: executes all defined unit tests.
main :: IO ()
main = do
  _ <- runTestTT (TestList [
      testEmpty,
      testInsertSingle,
      testInsertMultiple,
      testLookupSingle,
      testLookupMissing,
      testCountIfLengthGT3
--      testDeleteAndLookup       
    ])
  return ()


-- Test lookup of a key that does not exist in the BST.
-- This test will initially fail until 'lookupBST' handles missing keys correctly.
testLookupMissing :: Test
testLookupMissing =
  let tree = insert 10 "ten" (insert 5 "five" Empty)
      result = lookupBST 99 tree
  in TestCase (assertEqual "Lookup missing key should return Nothing" Nothing result)

-- Test counting values that match a predicate
-- Demonstrates higher-order functions and tree traversal
testCountIfLengthGT3 :: Test
testCountIfLengthGT3 =
  let tree = insert 1 "one" (insert 2 "three" (insert 3 "seven" Empty))
      result = countIf (\v -> length v > 3) tree
  in TestCase (assertEqual "Count values with length > 3" 2 result)

-- Test deletion of a key from the BST
-- Will fail until 'deleteBST' is implemented
--testDeleteAndLookup :: Test
--testDeleteAndLookup =
--  let tree = insert 1 "one" (insert 2 "two" (insert 3 "three" Empty))
--      updatedTree = deleteBST 2 tree
--      result = lookupBST 2 updatedTree
-- in TestCase (assertEqual "After deleting key 2, lookup should return Nothing" Nothing result)

