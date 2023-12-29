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
import Data.Aeson (encode)
import Data.Aeson.Text (encodeToTextBuilder)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Encoding as TE
import qualified Domain.Action.UI.Search.Common as DSearchCommon
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.Beckn.Gps as Gps
import Kernel.Types.Common
import qualified Tools.Maps as Maps

mkRouteInfoTags :: Maybe Meters -> Maybe Seconds -> Maybe [Maps.LatLong] -> Maybe [Spec.TagGroup]
mkRouteInfoTags distance duration mbPoints =
  Just
    [ Spec.TagGroup
        { tagGroupDescriptor =
            Just $
              Spec.Descriptor
                { descriptorText = Just "route_info",
                  descriptorName = Just "Route Information",
                  descriptorShortDesc = Nothing
                },
          tagGroupDisplay = Just False,
          tagGroupList =
            Just
              [ Spec.Tag
                  { tagDescriptor =
                      Just $
                        Spec.Descriptor
                          { descriptorText = (\_ -> Just "distance_info_in_m") =<< distance,
                            descriptorName = (\_ -> Just "Distance Information In Meters") =<< distance,
                            descriptorShortDesc = Nothing
                          },
                    tagDisplay = (\_ -> Just False) =<< distance,
                    tagValue = (\distanceInM -> Just $ show distanceInM.getMeters) =<< distance
                  },
                Spec.Tag
                  { tagDescriptor =
                      Just $
                        Spec.Descriptor
                          { descriptorText = (\_ -> Just "duration_info_in_s") =<< duration,
                            descriptorName = (\_ -> Just "Duration Information In Seconds") =<< duration,
                            descriptorShortDesc = Nothing
                          },
                    tagDisplay = (\_ -> Just False) =<< duration,
                    tagValue = (\durationInS -> Just $ show durationInS.getSeconds) =<< duration
                  },
                Spec.Tag
                  { tagDescriptor =
                      Just $
                        Spec.Descriptor
                          { descriptorText = (\_ -> Just "route_points") =<< mbPoints,
                            descriptorName = (\_ -> Just "Route Points") =<< mbPoints,
                            descriptorShortDesc = Nothing
                          },
                    tagDisplay = (\_ -> Just False) =<< mbPoints,
                    tagValue = (Just . LT.toStrict . TE.decodeUtf8 . encode) =<< mbPoints
                  }
              ]
        }
    ]

mkCustomerInfoTags :: Maybe Maps.Language -> Maybe Text -> Maybe Text -> Maybe [Spec.TagGroup]
mkCustomerInfoTags customerLanguage disabilityTag mbPhoneNumber =
  Just
    [ Spec.TagGroup
        { tagGroupDescriptor =
            Just $
              Spec.Descriptor
                { descriptorText = Just "customer_info",
                  descriptorName = Just "Customer Information",
                  descriptorShortDesc = Nothing
                },
          tagGroupDisplay = Just False,
          tagGroupList =
            Just
              [ Spec.Tag
                  { tagDescriptor =
                      Just $
                        Spec.Descriptor
                          { descriptorText = (\_ -> Just "customer_language") =<< customerLanguage,
                            descriptorName = (\_ -> Just "Customer Language") =<< customerLanguage,
                            descriptorShortDesc = Nothing
                          },
                    tagDisplay = (\_ -> Just False) =<< customerLanguage,
                    tagValue = (Just . show) =<< customerLanguage
                  },
                Spec.Tag
                  { tagDescriptor =
                      Just $
                        Spec.Descriptor
                          { descriptorText = (\_ -> Just "customer_disability") =<< disabilityTag,
                            descriptorName = (\_ -> Just "Customer Disability") =<< disabilityTag,
                            descriptorShortDesc = Nothing
                          },
                    tagDisplay = (\_ -> Just False) =<< disabilityTag,
                    tagValue = (Just . show) =<< disabilityTag
                  },
                Spec.Tag
                  { tagDescriptor =
                      Just $
                        Spec.Descriptor
                          { descriptorText = (\_ -> Just "customer_phone_number") =<< mbPhoneNumber,
                            descriptorName = (\_ -> Just "Customer Phone Number") =<< mbPhoneNumber,
                            descriptorShortDesc = Nothing
                          },
                    tagDisplay = (\_ -> Just False) =<< mbPhoneNumber,
                    tagValue = (Just . show) =<< mbPhoneNumber
                  }
              ]
        }
    ]

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
                      locationAreaText = origin.address.areaCode,
                      locationCity = Just $ Spec.City Nothing origin.address.city,
                      locationCountry = Just $ Spec.Country Nothing origin.address.country,
                      locationGps = Just . LT.toStrict . TB.toLazyText . encodeToTextBuilder $ toJSON originGps, -- JAYPAL, proper encoding to json is not happening.
                      locationState = Just $ Spec.State origin.address.state,
                      locationText = Nothing -- JAYPAL, Not sure what to keep here
                    },
              stopType = Just "START"
            },
          Spec.Stop
            { stopLocation =
                Just $
                  Spec.Location
                    { locationAddress = origin.address.building, -- JAYPAL, Confirm if it is correct to put it here
                      locationAreaText = destination.address.areaCode,
                      locationCity = Just $ Spec.City Nothing destination.address.city,
                      locationCountry = Just $ Spec.Country Nothing destination.address.country,
                      locationGps = Just . LT.toStrict . TE.decodeUtf8 $ encode destinationGps, -- JAYPAL, proper encoding to json is not happening.
                      locationState = Just $ Spec.State destination.address.state,
                      locationText = Nothing -- JAYPAL, Not sure what to keep here
                    },
              stopType = Just "END"
            }
        ]
