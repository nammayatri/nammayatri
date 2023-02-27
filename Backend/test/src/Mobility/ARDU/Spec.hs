{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Mobility.ARDU.Spec where

import EulerHS.Prelude
import qualified Mobility.ARDU.CancelFlow as DC
import Mobility.ARDU.DriverAcceptsNonrelevantQuote as NQ
import qualified Mobility.ARDU.DriverOffersTwice as OF
import qualified Mobility.ARDU.HealthCheck as HC
import qualified Mobility.ARDU.MapsConfig as MapsConfig
import qualified Mobility.ARDU.NearestDrivers as ND
import qualified Mobility.ARDU.SuccessFlow as SF
import qualified Mobility.ARDU.SyncRide as SR
import Test.Tasty
import Test.Tasty.Hspec (testSpec)

mkTestTree :: IO TestTree
mkTestTree = do
  hcSpec <- testSpec "HealthCheck" HC.spec
  mapsCfgSpec <- testSpec "MapsConfig" MapsConfig.spec
  sfSpec <- testSpec "SuccessFlow" SF.spec
  nqSpec <- testSpec "SecondDriver" NQ.spec
  otSpec <- testSpec "OffersTwice" OF.spec
  dcSpec <- testSpec "CancelFlow" DC.spec
  ndSpec <- testSpec "NearestDrivers" ND.spec
  srSpec <- testSpec "SyncRide" SR.spec
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
              nqSpec,
              otSpec,
              dcSpec,
              sfSpec,
              srSpec
            ]
      ]
