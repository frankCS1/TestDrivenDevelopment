-- BSTTests.hs
-- Author: Frank 
-- Contains unit tests for the BST module using HUnit.
-- This supports test-driven development (TDD) by verifying expected tree behaviours.

module Main where

import Test.HUnit   -- Import HUnit testing framework
import BST          -- Import the module under test

-- Test: Check that the Empty tree equals itself.
-- This serves as a minimal test to verify correct module setup and data structure equality.
testEmpty :: Test
testEmpty = TestCase (assertEqual "Empty tree should equal Empty" Empty Empty)

-- Main test runner: executes all defined unit tests.
-- Additional tests should be added to the TestList as new functionality is implemented.
main :: IO ()
main = do
  _ <- runTestTT (TestList [testEmpty])
  return ()
