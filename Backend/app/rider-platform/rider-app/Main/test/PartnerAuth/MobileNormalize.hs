{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for BHIM mobile-number normalisation. Guards against the
-- duplicate-account risk: BHIM may send the number bare, or with a 91 / +91 /
-- 0091 prefix, and it must normalise to the same ("+91", <local 10-digit>) used
-- to look up / create the Person. Malformed numbers must fail closed (Nothing)
-- so the facade never creates a Person under a junk identifier.
module PartnerAuth.MobileNormalize (tests) where

import Domain.Action.UI.PartnerAuth (normalizeIndianMobile)
import Test.Tasty
import Test.Tasty.HUnit
import Prelude

tests :: TestTree
tests =
  testGroup
    "normalizeIndianMobile"
    [ testGroup
        "accepts plausible Indian mobiles"
        [ testCase "bare 10-digit" $ normalizeIndianMobile "9876543210" @?= Just ("+91", "9876543210"),
          testCase "91 prefix" $ normalizeIndianMobile "919876543210" @?= Just ("+91", "9876543210"),
          testCase "+91 prefix" $ normalizeIndianMobile "+919876543210" @?= Just ("+91", "9876543210"),
          testCase "+91 with spaces" $ normalizeIndianMobile "+91 98765 43210" @?= Just ("+91", "9876543210"),
          testCase "0091 prefix" $ normalizeIndianMobile "00919876543210" @?= Just ("+91", "9876543210"),
          testCase "leading 6 boundary" $ normalizeIndianMobile "6000000000" @?= Just ("+91", "6000000000")
        ],
      testGroup
        "rejects malformed numbers (fail closed)"
        [ testCase "too few digits" $ normalizeIndianMobile "12345" @?= Nothing,
          testCase "empty" $ normalizeIndianMobile "" @?= Nothing,
          testCase "non-mobile leading digit (0)" $ normalizeIndianMobile "0123456789" @?= Nothing,
          testCase "non-mobile leading digit (5)" $ normalizeIndianMobile "5123456789" @?= Nothing,
          testCase "all non-digits" $ normalizeIndianMobile "not-a-number" @?= Nothing
        ]
    ]
