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
testEmpty = TestCase (assertEqual "Empty tree should equal Empty" Empty Empty)

-- Test inserting a single key-value pair into an empty BST.
-- This test was written before the insert function existed,
-- demonstrating the first step in the TDD cycle.
testInsertSingle :: Test
testInsertSingle =
  let result = insert 5 "five" Empty
      expected = Node 5 "five" Empty Empty
  in TestCase (assertEqual "Insert single node into empty BST" expected result)

-- Add more tests progressively as we implement new features (TDD process).
main :: IO ()
main = do
  _ <- runTestTT (TestList [testEmpty, testInsertSingle])
  return ()
