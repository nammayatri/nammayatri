module Amount where

import Beckn.Types.Amount
import Data.Aeson
import Data.Ratio
import EulerHS.Prelude
import Test.Tasty
import Test.Tasty.HUnit

integerToString :: TestTree
integerToString =
  testCase "Integer to string" $
    rationalToString 5 (1 % 1) @?= Just "1"

largeIntegerToString :: TestTree
largeIntegerToString =
  testCase "Large integer to string" $
    rationalToString 5 (100000 % 1) @?= Nothing

infiniteFractionToString :: TestTree
infiniteFractionToString =
  testCase "Infinite fraction to string" $
    rationalToString 5 (1000 % 3) @?= Just "333.33"

validateNormalInput :: TestTree
validateNormalInput =
  testCase "Normal input" $
    validate 5 "-123.45" @?= True

validateTooLongInteger :: TestTree
validateTooLongInteger =
  testCase "Too long integer" $
    validate 5 "100000" @?= False

validateTooLongFraction :: TestTree
validateTooLongFraction =
  testCase "Too long fraction" $
    validate 5 "0.33333" @?= False

parseSuccess :: TestTree
parseSuccess = testCase "Parse success" $ do
  let result = fromJSON (String "1.25") :: Result Amount
  case result of
    Error _ -> assertFailure "Parsing of \"1.25\" should succeed"
    Success _ -> pure ()

parseError :: TestTree
parseError = testCase "Parse error" $ do
  let result = fromJSON (String "abc") :: Result Amount
  case result of
    Error err -> err @?= "Cannot parse abc as a monetary amount."
    Success _ -> assertFailure "Parsing of abc as a number should fail."

amountTests :: TestTree
amountTests =
  testGroup
    "Amount tests"
    [ integerToString,
      largeIntegerToString,
      infiniteFractionToString,
      validateNormalInput,
      validateTooLongInteger,
      validateTooLongFraction,
      parseSuccess,
      parseError
    ]
