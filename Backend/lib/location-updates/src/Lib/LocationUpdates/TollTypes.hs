{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.LocationUpdates.TollTypes
  ( TollChargeDetails (..),
    TollInfo (..),
    emptyTollChargeDetails,
    emptyTollInfo,
    estimatedTollInfoFromCharges,
    hasDetectedTolls,
  )
where

import Kernel.Prelude
import Kernel.Types.Common

data TollChargeDetails = TollChargeDetails
  { tollCharges :: HighPrecMoney,
    tollNames :: [Text],
    tollIds :: [Text]
  }
  deriving (Show, Eq, Generic)

data TollInfo = TollInfo
  { tollCharges :: HighPrecMoney,
    tollNames :: [Text],
    tollIds :: [Text],
    isAutoRickshawAllowed :: Bool,
    isTwoWheelerAllowed :: Maybe Bool,
    autoRickshawTollChargeDetails :: TollChargeDetails,
    twoWheelerTollChargeDetails :: TollChargeDetails
  }
  deriving (Show, Eq, Generic)

emptyTollChargeDetails :: TollChargeDetails
emptyTollChargeDetails = TollChargeDetails 0 [] []

emptyTollInfo :: TollInfo
emptyTollInfo =
  TollInfo
    { tollCharges = 0,
      tollNames = [],
      tollIds = [],
      isAutoRickshawAllowed = True,
      isTwoWheelerAllowed = Just True,
      autoRickshawTollChargeDetails = emptyTollChargeDetails,
      twoWheelerTollChargeDetails = emptyTollChargeDetails
    }

estimatedTollInfoFromCharges :: HighPrecMoney -> [Text] -> [Text] -> TollInfo
estimatedTollInfoFromCharges charges names ids =
  let details = TollChargeDetails charges names ids
   in TollInfo
        { tollCharges = charges,
          tollNames = names,
          tollIds = ids,
          isAutoRickshawAllowed = True,
          isTwoWheelerAllowed = Just True,
          autoRickshawTollChargeDetails = details,
          twoWheelerTollChargeDetails = details
        }

hasDetectedTolls :: TollInfo -> Bool
hasDetectedTolls TollInfo {..} = tollCharges > 0 && not (null tollNames)
