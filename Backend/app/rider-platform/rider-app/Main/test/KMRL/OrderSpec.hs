{-# LANGUAGE OverloadedStrings #-}

module KMRL.OrderSpec (tests) where

import EulerHS.Prelude
import ExternalBPP.ExternalAPI.Metro.KMRL.Order (toKMRLTransactionId, transformKochiStatus)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "KMRL order values"
    [ testGroup "transactionId carries the BAP prefix that makes it unique" transactionIdTests,
      testGroup "ticket status is translated, not passed through" statusTests
    ]

transactionIdTests :: [TestTree]
transactionIdTests =
  [ testCase "four characters of the buyer's domain lead the id" $
      toKMRLTransactionId "gateway.rapido.bike" "abc-123" @?= "RAPIABC123",
    testCase "a scheme and path on the bap_id do not reach the prefix" $
      toKMRLTransactionId "https://gateway.rapido.bike/buyer/api" "abc-123" @?= "RAPIABC123",
    testCase "our own traffic is prefixed TRF plus three of the bap_id" $
      toKMRLTransactionId "triffy.co.in" "abc-123" @?= "TRFTRIABC123",
    testCase "a domain shorter than the prefix does not blow up" $
      toKMRLTransactionId "ab.in" "xy1" @?= "ABXY1",
    testCase "separators are stripped, so ids must differ as alphanumerics" $
      toKMRLTransactionId "gateway.rapido.bike" "ab-c"
        @?= toKMRLTransactionId "gateway.rapido.bike" "abc"
  ]

statusTests :: [TestTree]
statusTests =
  [ testCase "UNUSED is an unclaimed ticket" $ transformKochiStatus "UNUSED" @?= Just "UNCLAIMED",
    testCase "USED is a claimed ticket" $ transformKochiStatus "USED" @?= Just "CLAIMED",
    testCase "CANCELLED passes through" $ transformKochiStatus "CANCELLED" @?= Just "CANCELLED",
    testCase "EXPIRED passes through" $ transformKochiStatus "EXPIRED" @?= Just "EXPIRED",
    testCase "case from the operator does not matter" $ transformKochiStatus "unused" @?= Just "UNCLAIMED",
    testCase "an unknown status is Nothing, never a crash" $ transformKochiStatus "TELEPORTED" @?= Nothing
  ]
