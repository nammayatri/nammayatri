{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.OnDemand.Utils.OnSearch where

-- import Beckn.ACL.Common (getTagV2)

-- import Data.Aeson
-- import qualified Data.Text as T
-- import qualified EulerHS.Language as L

-- import Kernel.External.Maps as Maps
-- import qualified Kernel.Types.Beckn.Context as Context
-- import Kernel.Types.Common
-- import qualified Kernel.Types.Error as Error
-- import Kernel.Utils.Common
-- import Tools.Error

import qualified Beckn.Types.Core.Taxi.OnSearch as OS
import qualified BecknV2.OnDemand.Types as Spec
import Control.Lens
import qualified Data.Aeson as A
import qualified Data.List as List
import Domain.Action.Beckn.Search
import Domain.Types.Estimate
import EulerHS.Prelude hiding (id, view, (^?))
import GHC.Float (double2Int)
import Kernel.Types.Beckn.DecimalValue as DecimalValue

mkProviderLocations :: [EstimateInfo] -> [Spec.Location]
mkProviderLocations estimatesList =
  foldl (<>) [] $ map mkProviderLocation estimatesList

mkProviderLocation :: EstimateInfo -> [Spec.Location]
mkProviderLocation EstimateInfo {..} = do
  let locationGps = A.decode $ A.encode driverLatLongs
  [ Spec.Location
      { locationAddress = Nothing,
        locationAreaCode = Nothing,
        locationCity = Nothing,
        locationCountry = Nothing,
        locationGps = locationGps,
        locationId = Nothing,
        locationState = Nothing
      }
    ]

mkItemTags :: EstimateInfo -> [Spec.TagGroup]
mkItemTags estInfo = do
  let estimate = estInfo.estimate
  [mkGeneralInfoTag estimate estInfo, mkFarePolicyTag estimate, mkRateCardTag estimate]

mkGeneralInfoTag :: Estimate -> EstimateInfo -> Spec.TagGroup
mkGeneralInfoTag estimate estInfo =
  let specialLocationTag = estimate.specialLocationTag
   in Spec.TagGroup
        { tagGroupDisplay = Just False,
          tagGroupDescriptor =
            Just
              Spec.Descriptor
                { descriptorCode = Just "general_info",
                  descriptorName = Just "General Information",
                  descriptorShortDesc = Nothing
                },
          tagGroupList =
            Just $
              specialLocationTagSingleton specialLocationTag
                ++ distanceToNearestDriverTagSingleton estInfo.distanceToNearestDriver
        }
  where
    specialLocationTagSingleton specialLocationTag
      | isNothing specialLocationTag = []
      | otherwise =
        List.singleton $
          Spec.Tag
            { tagDisplay = Just True,
              tagDescriptor =
                Just
                  Spec.Descriptor
                    { descriptorCode = Just "special_location_tag",
                      descriptorName = Just "Special Location Tag",
                      descriptorShortDesc = Nothing
                    },
              tagValue = specialLocationTag
            }
    distanceToNearestDriverTagSingleton distanceToNearestDriver =
      List.singleton $
        Spec.Tag
          { tagDisplay = Just False,
            tagDescriptor =
              Just
                Spec.Descriptor
                  { descriptorCode = Just "distance_to_nearest_driver",
                    descriptorName = Just "Distance To Nearest Driver",
                    descriptorShortDesc = Nothing
                  },
            tagValue = Just $ show . double2Int . realToFrac $ distanceToNearestDriver
          }

buildEstimateBreakUpListTags :: EstimateBreakup -> Spec.Tag
buildEstimateBreakUpListTags EstimateBreakup {..} = do
  Spec.Tag
    { tagDisplay = Just False,
      tagDescriptor =
        Just
          Spec.Descriptor
            { descriptorCode = Just title,
              descriptorName = Just title,
              descriptorShortDesc = Nothing
            },
      tagValue = Just $ show price.value.getMoney
    }

mkFarePolicyTag :: Estimate -> Spec.TagGroup
mkFarePolicyTag estimate = do
  let estimateBreakUpList = buildEstimateBreakUpListTags <$> estimate.estimateBreakupList
  Spec.TagGroup
    { tagGroupDisplay = Just False,
      tagGroupDescriptor =
        Just
          Spec.Descriptor
            { descriptorCode = Just "fare_breakup",
              descriptorName = Just "Fare Breakup",
              descriptorShortDesc = Nothing
            },
      tagGroupList = Just estimateBreakUpList
    }

mkRateCardTag :: Estimate -> Spec.TagGroup
mkRateCardTag estimate =
  let nightShiftCharges = (estimate.nightShiftInfo <&> (.nightShiftCharge))
      oldNightShiftCharges = (OS.DecimalValue . toRational <$> (estimate.nightShiftInfo <&> (.oldNightShiftCharge)))
      nightShiftStart = (estimate.nightShiftInfo <&> (.nightShiftStart))
      waitingChargePerMin = (estimate.waitingCharges.waitingChargePerMin)
      nightShiftEnd = (estimate.nightShiftInfo <&> (.nightShiftEnd))
   in Spec.TagGroup
        { tagGroupDisplay = Just False,
          tagGroupDescriptor =
            Just
              Spec.Descriptor
                { descriptorCode = Just "rate_card",
                  descriptorName = Just "Rate Card",
                  descriptorShortDesc = Nothing
                },
          tagGroupList =
            Just $
              nightShiftChargesTagSingleton nightShiftCharges
                ++ oldNightShiftChargesTagSingleton oldNightShiftCharges
                ++ nightShiftStartTagSingleton nightShiftStart
                ++ waitingChargePerMinTagSingleton waitingChargePerMin
                ++ nightShiftEndTagSingleton nightShiftEnd
        }
  where
    nightShiftChargesTagSingleton nightShiftCharges
      | isNothing nightShiftCharges = []
      | otherwise =
        List.singleton $
          Spec.Tag
            { tagDisplay = Just False,
              tagDescriptor =
                Just
                  Spec.Descriptor
                    { descriptorCode = Just "night_shift_charge",
                      descriptorName = Just "Night Shift Charges",
                      descriptorShortDesc = Nothing
                    },
              tagValue = (\charges -> Just $ show charges.getMoney) =<< nightShiftCharges
            }
    oldNightShiftChargesTagSingleton oldNightShiftCharges
      | isNothing oldNightShiftCharges = []
      | otherwise =
        List.singleton $
          Spec.Tag
            { tagDisplay = Just False,
              tagDescriptor =
                Just
                  Spec.Descriptor
                    { descriptorCode = Just "old_night_shift_charge",
                      descriptorName = Just "Old Night Shift Charges",
                      descriptorShortDesc = Nothing
                    },
              tagValue = (Just . DecimalValue.valueToString) =<< oldNightShiftCharges
            }
    nightShiftStartTagSingleton nightShiftStart
      | isNothing nightShiftStart = []
      | otherwise =
        List.singleton $
          Spec.Tag
            { tagDisplay = Just False,
              tagDescriptor =
                Just
                  Spec.Descriptor
                    { descriptorCode = Just "night_shift_start",
                      descriptorName = Just "Night Shift Start Timings",
                      descriptorShortDesc = Nothing
                    },
              tagValue = (Just . show) =<< nightShiftStart
            }
    waitingChargePerMinTagSingleton waitingChargePerMin
      | isNothing waitingChargePerMin = []
      | otherwise =
        List.singleton $
          Spec.Tag
            { tagDisplay = Just False,
              tagDescriptor =
                Just
                  Spec.Descriptor
                    { descriptorCode = Just "waiting_charge_per_min",
                      descriptorName = Just "Waiting Charges Per Min",
                      descriptorShortDesc = Nothing
                    },
              tagValue = (\charges -> Just $ show charges.getMoney) =<< waitingChargePerMin
            }
    nightShiftEndTagSingleton nightShiftEnd
      | isNothing nightShiftEnd = []
      | otherwise =
        List.singleton $
          Spec.Tag
            { tagDisplay = Just False,
              tagDescriptor =
                Just
                  Spec.Descriptor
                    { descriptorCode = Just "night_shift_end",
                      descriptorName = Just "Night Shift End Timings",
                      descriptorShortDesc = Nothing
                    },
              tagValue = (Just . show) =<< nightShiftEnd
            }
