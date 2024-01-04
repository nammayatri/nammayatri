{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.OnDemand.Utils.Search where

import qualified BecknV2.OnDemand.Types as Spec
import qualified Data.Aeson as A
import qualified Data.List as List
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as TE
import EulerHS.Prelude hiding (id)
import Kernel.Types.Common
import qualified Tools.Maps as Maps

mkRouteInfoTags :: Maybe Meters -> Maybe Seconds -> Maybe [Maps.LatLong] -> Maybe [Spec.TagGroup]
mkRouteInfoTags distance duration mbPoints =
  Just
    [ Spec.TagGroup
        { tagGroupDescriptor =
            Just $
              Spec.Descriptor
                { descriptorCode = Just "route_info",
                  descriptorName = Just "Route Information",
                  descriptorShortDesc = Nothing
                },
          tagGroupDisplay = Just False,
          tagGroupList =
            Just $
              distanceSingleton
                ++ durationSingleton
                ++ mbPointsSingleton
        }
    ]
  where
    distanceSingleton
      | isNothing distance = []
      | otherwise =
        List.singleton $
          Spec.Tag
            { tagDescriptor =
                Just $
                  Spec.Descriptor
                    { descriptorCode = Just "distance_info_in_m",
                      descriptorName = Just "Distance Information In Meters",
                      descriptorShortDesc = Nothing
                    },
              tagDisplay = Just False,
              tagValue = (\distanceInM -> Just $ show distanceInM.getMeters) =<< distance
            }
    durationSingleton
      | isNothing duration = []
      | otherwise =
        List.singleton $
          Spec.Tag
            { tagDescriptor =
                Just $
                  Spec.Descriptor
                    { descriptorCode = Just "duration_info_in_s",
                      descriptorName = Just "Duration Information In Seconds",
                      descriptorShortDesc = Nothing
                    },
              tagDisplay = Just False,
              tagValue = (\durationInS -> Just $ show durationInS.getSeconds) =<< duration
            }
    mbPointsSingleton
      | isNothing mbPoints = []
      | otherwise =
        List.singleton $
          Spec.Tag
            { tagDescriptor =
                Just $
                  Spec.Descriptor
                    { descriptorCode = Just "route_points",
                      descriptorName = Just "Route Points",
                      descriptorShortDesc = Nothing
                    },
              tagDisplay = Just False,
              tagValue = (Just . LT.toStrict . TE.decodeUtf8 . A.encode) =<< mbPoints
            }

mkCustomerInfoTags :: Maybe Maps.Language -> Maybe Text -> Maybe Text -> Maybe [Spec.TagGroup]
mkCustomerInfoTags customerLanguage disabilityTag mbPhoneNumber =
  Just
    [ Spec.TagGroup
        { tagGroupDescriptor =
            Just $
              Spec.Descriptor
                { descriptorCode = Just "customer_info",
                  descriptorName = Just "Customer Information",
                  descriptorShortDesc = Nothing
                },
          tagGroupDisplay = Just False,
          tagGroupList =
            Just $
              customerLanguageSingleton
                ++ disabilityTagSingleton
                ++ mbPhoneNumberSingleton
        }
    ]
  where
    customerLanguageSingleton
      | isNothing customerLanguage = []
      | otherwise =
        List.singleton $
          Spec.Tag
            { tagDescriptor =
                Just $
                  Spec.Descriptor
                    { descriptorCode = Just "customer_language",
                      descriptorName = Just "Customer Language",
                      descriptorShortDesc = Nothing
                    },
              tagDisplay = Just False,
              tagValue = (Just . show) =<< customerLanguage
            }
    disabilityTagSingleton
      | isNothing disabilityTag = []
      | otherwise =
        List.singleton $
          Spec.Tag
            { tagDescriptor =
                Just $
                  Spec.Descriptor
                    { descriptorCode = Just "customer_disability",
                      descriptorName = Just "Customer Disability",
                      descriptorShortDesc = Nothing
                    },
              tagDisplay = Just False,
              tagValue = (A.decode . A.encode) =<< disabilityTag
            }
    mbPhoneNumberSingleton
      | isNothing mbPhoneNumber = []
      | otherwise =
        List.singleton $
          Spec.Tag
            { tagDescriptor =
                Just $
                  Spec.Descriptor
                    { descriptorCode = Just "customer_phone_number",
                      descriptorName = Just "Customer Phone Number",
                      descriptorShortDesc = Nothing
                    },
              tagDisplay = Just False,
              tagValue = (A.decode . A.encode) =<< mbPhoneNumber
            }
