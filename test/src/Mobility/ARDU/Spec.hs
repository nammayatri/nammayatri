module Mobility.ARDU.Spec where

import EulerHS.Prelude
import qualified Mobility.ARDU.CancelFlow as DC
import Mobility.ARDU.DriverAcceptsNonrelevantQuote as NQ
import qualified Mobility.ARDU.DriverOffersTwice as OF
import qualified Mobility.ARDU.HealthCheck as HC
import qualified Mobility.ARDU.MapsConfig as MapsConfig
import qualified Mobility.ARDU.NearestDrivers as ND
import qualified Mobility.ARDU.SuccessFlow as SF
import Test.Tasty
import Test.Tasty.Hspec hiding (after)

mkTestTree :: IO TestTree
mkTestTree = do
  hcSpec <- testSpec "HealthCheck" HC.spec
  mapsCfgSpec <- testSpec "MapsConfig" MapsConfig.spec
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
            "Merchant configs"
            [mapsCfgSpec],
        after AllSucceed "Merchant configs" $
          testGroup
            "APIs"
            [ ndSpec,
              otSpec,
              dcSpec,
              sfSpec,
              nqSpec
            ]
      ]
