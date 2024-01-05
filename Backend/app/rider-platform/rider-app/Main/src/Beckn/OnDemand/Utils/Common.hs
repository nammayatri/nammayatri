{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}

module Beckn.OnDemand.Utils.Common where

import qualified BecknV2.OnDemand.Types as Spec
import Control.Lens ((%~))
import qualified Data.Aeson as A
import qualified Data.List as List
import qualified Data.Text as T
import qualified Domain.Action.UI.Search.Common as DSearchCommon
import qualified Domain.Types.Merchant as DM
import EulerHS.Prelude hiding (id, (%~))
import qualified Kernel.Prelude as KP
import Kernel.Types.App
import qualified Kernel.Types.Beckn.Gps as Gps
import Kernel.Types.Id
import Kernel.Utils.Common

mkBapUri :: (HasFlowEnv m r '["nwAddress" ::: BaseUrl]) => Id DM.Merchant -> m KP.BaseUrl
mkBapUri merchantId = asks (.nwAddress) <&> #baseUrlPath %~ (<> "/" <> T.unpack merchantId.getId)

mkStops :: DSearchCommon.SearchReqLocation -> DSearchCommon.SearchReqLocation -> Maybe [Spec.Stop]
mkStops origin destination =
  let originGps = Gps.Gps {lat = origin.gps.lat, lon = origin.gps.lon}
      destinationGps = Gps.Gps {lat = destination.gps.lat, lon = destination.gps.lon}
   in Just
        [ Spec.Stop
            { stopLocation =
                Just $
                  Spec.Location
                    { locationAddress = origin.address.building, -- JAYPAL, Confirm if it is correct to put it here
                      locationAreaCode = origin.address.areaCode,
                      locationCity = Just $ Spec.City Nothing origin.address.city,
                      locationCountry = Just $ Spec.Country Nothing origin.address.country,
                      locationGps = A.decode $ A.encode originGps,
                      locationState = Just $ Spec.State origin.address.state,
                      locationId = Nothing -- JAYPAL, Not sure what to keep here
                    },
              stopType = Just "START"
            },
          Spec.Stop
            { stopLocation =
                Just $
                  Spec.Location
                    { locationAddress = origin.address.building, -- JAYPAL, Confirm if it is correct to put it here
                      locationAreaCode = destination.address.areaCode,
                      locationCity = Just $ Spec.City Nothing destination.address.city,
                      locationCountry = Just $ Spec.Country Nothing destination.address.country,
                      locationGps = A.decode $ A.encode destinationGps,
                      locationState = Just $ Spec.State destination.address.state,
                      locationId = Nothing -- JAYPAL, Not sure what to keep here
                    },
              stopType = Just "END"
            }
        ]

mkPaymentTags :: Maybe [Spec.TagGroup]
mkPaymentTags =
  Just
    [ Spec.TagGroup
        { tagGroupDescriptor =
            Just $
              Spec.Descriptor
                { descriptorCode = Just "BUYER_FINDER_FEES",
                  descriptorName = Nothing,
                  descriptorShortDesc = Nothing
                },
          tagGroupDisplay = Just False,
          tagGroupList = Just buyerFinderFeesSingleton
        },
      Spec.TagGroup
        { tagGroupDescriptor =
            Just $
              Spec.Descriptor
                { descriptorCode = Just "SETTLEMENT_TERMS",
                  descriptorName = Nothing,
                  descriptorShortDesc = Nothing
                },
          tagGroupDisplay = Just False,
          tagGroupList =
            Just $
              delayInterestSingleton
                ++ settlementTypeSingleton
                ++ staticTermsSingleton
        }
    ]
  where
    buyerFinderFeesSingleton =
      List.singleton $
        Spec.Tag
          { tagDescriptor =
              Just $
                Spec.Descriptor
                  { descriptorCode = Just "BUYER_FINDER_FEES_PERCENTAGE",
                    descriptorName = Nothing,
                    descriptorShortDesc = Nothing
                  },
            tagDisplay = Just False,
            tagValue = Just "0"
          }
    delayInterestSingleton =
      List.singleton $
        Spec.Tag
          { tagDescriptor =
              Just $
                Spec.Descriptor
                  { descriptorCode = Just "DELAY_INTEREST",
                    descriptorName = Nothing,
                    descriptorShortDesc = Nothing
                  },
            tagDisplay = Just False,
            tagValue = Just "0"
          }
    settlementTypeSingleton =
      List.singleton $
        Spec.Tag
          { tagDescriptor =
              Just $
                Spec.Descriptor
                  { descriptorCode = Just "SETTLEMENT_TYPE",
                    descriptorName = Nothing,
                    descriptorShortDesc = Nothing
                  },
            tagDisplay = Just False,
            tagValue = Just "RSF"
          }
    staticTermsSingleton =
      List.singleton $
        Spec.Tag
          { tagDescriptor =
              Just $
                Spec.Descriptor
                  { descriptorCode = Just "STATIC_TERMS",
                    descriptorName = Nothing,
                    descriptorShortDesc = Nothing
                  },
            tagDisplay = Just False,
            tagValue = Just "https://example-test-bap.com/static-terms.txt"
          }
