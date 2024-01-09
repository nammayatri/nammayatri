{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.OnDemand.Utils.OnUpdate where

import qualified BecknV2.OnDemand.Types as Spec
import qualified Data.Aeson as A
import qualified Data.List as List
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Vehicle as DVeh
import EulerHS.Prelude hiding (id, (%~))
import qualified Kernel.Types.Beckn.Gps as Gps

mkStops :: DBooking.Booking -> Text -> Maybe [Spec.Stop]
mkStops booking rideOtp =
  let origin = booking.fromLocation
      destination = booking.toLocation
      originGps = Gps.Gps {lat = origin.lat, lon = origin.lon}
      destinationGps = Gps.Gps {lat = destination.lat, lon = destination.lon}
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
              stopType = Just "START",
              stopAuthorization =
                Just $
                  Spec.Authorization
                    { authorizationToken = Just rideOtp,
                      authorizationType = Just "OTP"
                    }
            },
          Spec.Stop
            { stopLocation =
                Just $
                  Spec.Location
                    { locationAddress = destination.address.building, -- JAYPAL, Confirm if it is correct to put it here
                      locationAreaCode = destination.address.areaCode,
                      locationCity = Just $ Spec.City Nothing destination.address.city,
                      locationCountry = Just $ Spec.Country Nothing destination.address.country,
                      locationGps = A.decode $ A.encode destinationGps,
                      locationState = Just $ Spec.State destination.address.state,
                      locationId = Nothing -- JAYPAL, Not sure what to keep here
                    },
              stopType = Just "END",
              stopAuthorization = Nothing
            }
        ]

mkFulfillmentType :: DBooking.BookingType -> Text
mkFulfillmentType = \case
  DBooking.NormalBooking -> "RIDE"
  DBooking.SpecialZoneBooking -> "RIDE_OTP"

showVariant :: DVeh.Variant -> Maybe Text
showVariant = A.decode . A.encode

mkRideAssignedPersonTags :: SP.Person -> Maybe [Spec.TagGroup]
mkRideAssignedPersonTags driver =
  Just
    [ Spec.TagGroup
        { tagGroupDescriptor =
            Just $
              Spec.Descriptor
                { descriptorCode = Just "driver_details",
                  descriptorName = Just "Driver Details",
                  descriptorShortDesc = Nothing
                },
          tagGroupDisplay = Just False,
          tagGroupList =
            Just $
              registeredAtSingleton
                ++ driverRatingSingleton
        }
    ]
  where
    registeredAtSingleton =
      List.singleton $
        Spec.Tag
          { tagDescriptor =
              Just $
                Spec.Descriptor
                  { descriptorCode = Just "registered_at",
                    descriptorName = Just "Registered At",
                    descriptorShortDesc = Nothing
                  },
            tagDisplay = Just False,
            tagValue = Just $ show driver.createdAt
          }

    driverRatingSingleton
      | isNothing driver.rating = []
      | otherwise =
        List.singleton $
          Spec.Tag
            { tagDescriptor =
                Just $
                  Spec.Descriptor
                    { descriptorCode = Just "rating",
                      descriptorName = Just "rating",
                      descriptorShortDesc = Nothing
                    },
              tagDisplay = Just False,
              tagValue = show <$> driver.rating
            }
