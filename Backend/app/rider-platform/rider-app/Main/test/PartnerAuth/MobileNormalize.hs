{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for BHIM mobile-number normalisation. Guards against the
-- duplicate-account risk: BHIM may send the number bare, or with a 91 / +91 /
-- 0091 prefix, and it must normalise to the same ("+91", <local 10-digit>) used
-- to look up / create the Person.
module PartnerAuth.MobileNormalize (tests) where

import Domain.Action.UI.PartnerAuth (normalizeIndianMobile)
import Test.Tasty
import Test.Tasty.HUnit
import Prelude

tests :: TestTree
tests =
  testGroup
    "normalizeIndianMobile"
    [ testCase "bare 10-digit" $ normalizeIndianMobile "9876543210" @?= ("+91", "9876543210"),
      testCase "91 prefix" $ normalizeIndianMobile "919876543210" @?= ("+91", "9876543210"),
      testCase "+91 prefix" $ normalizeIndianMobile "+919876543210" @?= ("+91", "9876543210"),
      testCase "+91 with spaces" $ normalizeIndianMobile "+91 98765 43210" @?= ("+91", "9876543210"),
      testCase "0091 prefix" $ normalizeIndianMobile "00919876543210" @?= ("+91", "9876543210")
    ]
