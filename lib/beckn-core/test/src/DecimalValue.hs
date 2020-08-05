module DecimalValue where

import Beckn.Types.Core.Amount
import Beckn.Types.Core.DecimalValue
import Data.Ratio
import EulerHS.Prelude
import Test.Tasty
import Test.Tasty.HUnit

amountToDecimalValue :: TestTree
amountToDecimalValue =
  testCase "Amount to DecimalValue" $ do
    _integral decimalValue @?= "10"
    _fractional decimalValue @?= Just "5"
  where
    decimalValue = convertAmountToDecimalValue $ Amount 10.5

decimalValueToAmount :: TestTree
decimalValueToAmount =
  testCase "DecimalValue to Amount" $
    do
      convertDecimalValueToAmount (DecimalValue "10" (Just "5"))
        @?= Just amount
  where
    amount = Amount (21 % 2)

negativeAmountToDecimalValue :: TestTree
negativeAmountToDecimalValue =
  testCase "Negative amount to DecimalValue" $ do
    _integral decimalValue @?= "-10"
    _fractional decimalValue @?= Nothing
  where
    decimalValue = convertAmountToDecimalValue $ Amount (-10)

negativeDecimalValueToAmount :: TestTree
negativeDecimalValueToAmount =
  testCase "Negative DecimalValue to Amount" $ do
    convertDecimalValueToAmount (DecimalValue "-10" Nothing) @?= Just amount
  where
    amount = Amount (-10 % 1)

decimalValueTests :: TestTree
decimalValueTests =
  testGroup
    "DecimalValue tests"
    [ amountToDecimalValue,
      decimalValueToAmount,
      negativeAmountToDecimalValue,
      negativeDecimalValueToAmount
    ]
