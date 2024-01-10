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
            Just
              [ Spec.Tag
                  { tagDisplay = (\_ -> Just True) =<< specialLocationTag,
                    tagDescriptor =
                      ( \_ ->
                          Just
                            Spec.Descriptor
                              { descriptorCode = (\_ -> Just "special_location_tag") =<< specialLocationTag,
                                descriptorName = (\_ -> Just "Special Location Tag") =<< specialLocationTag,
                                descriptorShortDesc = Nothing
                              }
                      )
                        =<< specialLocationTag,
                    tagValue = specialLocationTag
                  },
                Spec.Tag
                  { tagDisplay = Just False,
                    tagDescriptor =
                      Just
                        Spec.Descriptor
                          { descriptorCode = Just "distance_to_nearest_driver",
                            descriptorName = Just "Distance To Nearest Driver",
                            descriptorShortDesc = Nothing
                          },
                    tagValue = Just $ show . double2Int . realToFrac $ estInfo.distanceToNearestDriver
                  }
              ]
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
            Just
              [ Spec.Tag
                  { tagDisplay = (\_ -> Just False) =<< nightShiftCharges,
                    tagDescriptor =
                      ( \_ ->
                          Just
                            Spec.Descriptor
                              { descriptorCode = (\_ -> Just "night_shift_charge") =<< nightShiftCharges,
                                descriptorName = (\_ -> Just "Night Shift Charges") =<< nightShiftCharges,
                                descriptorShortDesc = Nothing
                              }
                      )
                        =<< nightShiftCharges,
                    tagValue = (\charges -> Just $ show charges.getMoney) =<< nightShiftCharges
                  },
                Spec.Tag
                  { tagDisplay = (\_ -> Just False) =<< oldNightShiftCharges,
                    tagDescriptor =
                      ( \_ ->
                          Just
                            Spec.Descriptor
                              { descriptorCode = (\_ -> Just "old_night_shift_charge") =<< oldNightShiftCharges,
                                descriptorName = (\_ -> Just "Old Night Shift Charges") =<< oldNightShiftCharges,
                                descriptorShortDesc = Nothing
                              }
                      )
                        =<< oldNightShiftCharges,
                    tagValue = (Just . DecimalValue.valueToString) =<< oldNightShiftCharges
                  },
                Spec.Tag
                  { tagDisplay = (\_ -> Just False) =<< nightShiftStart,
                    tagDescriptor =
                      ( \_ ->
                          Just
                            Spec.Descriptor
                              { descriptorCode = (\_ -> Just "night_shift_start") =<< nightShiftStart,
                                descriptorName = (\_ -> Just "Night Shift Start Timings") =<< nightShiftStart,
                                descriptorShortDesc = Nothing
                              }
                      )
                        =<< nightShiftStart,
                    tagValue = (Just . show) =<< nightShiftStart
                  },
                Spec.Tag
                  { tagDisplay = (\_ -> Just False) =<< waitingChargePerMin,
                    tagDescriptor =
                      ( \_ ->
                          Just
                            Spec.Descriptor
                              { descriptorCode = (\_ -> Just "waiting_charge_per_min") =<< waitingChargePerMin,
                                descriptorName = (\_ -> Just "Waiting Charges Per Min") =<< waitingChargePerMin,
                                descriptorShortDesc = Nothing
                              }
                      )
                        =<< waitingChargePerMin,
                    tagValue = (\charges -> Just $ show charges.getMoney) =<< waitingChargePerMin
                  },
                Spec.Tag
                  { tagDisplay = (\_ -> Just False) =<< nightShiftEnd,
                    tagDescriptor =
                      ( \_ ->
                          Just
                            Spec.Descriptor
                              { descriptorCode = (\_ -> Just "night_shift_end") =<< nightShiftEnd,
                                descriptorName = (\_ -> Just "Night Shift End Timings") =<< nightShiftEnd,
                                descriptorShortDesc = Nothing
                              }
                      )
                        =<< nightShiftEnd,
                    tagValue = (Just . show) =<< nightShiftEnd
                  }
              ]
        }
