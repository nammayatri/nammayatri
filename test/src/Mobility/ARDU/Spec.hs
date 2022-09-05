module Mobility.ARDU.Spec where

import EulerHS.Prelude
import qualified Mobility.ARDU.CancelFlow as DC
import Mobility.ARDU.DriverAcceptsNonrelevantQuote as NQ
import qualified Mobility.ARDU.DriverOffersTwice as OF
import qualified Mobility.ARDU.HealthCheck as HC
import qualified Mobility.ARDU.NearestDrivers as ND
import qualified Mobility.ARDU.SuccessFlow as SF
import Test.Tasty
import Test.Tasty.Hspec hiding (after)

mkTestTree :: IO TestTree
mkTestTree = do
  hcSpec <- testSpec "HealthCheck" HC.spec
  sfSpec <- testSpec "SuccessFlow" SF.spec
  nqSpec <- testSpec "SecondDriver" NQ.spec
  otSpec <- testSpec "OffersTwice" OF.spec
  dcSpec <- testSpec "CancelFlow" DC.spec
  ndSpec <- testSpec "NearestDrivers" ND.spec
  ------------------------------------------------------------------
  return $
    testGroup
      "ARDU"
      [ hcSpec,
        after AllSucceed "HealthCheck" $
          testGroup
            "APIs"
            [ ndSpec,
              nqSpec,
              otSpec,
              dcSpec,
              sfSpec
            ]
      ]
