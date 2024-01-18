{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.OnDemand.Utils.OnUpdate where

import qualified Beckn.ACL.Common as Common
import qualified Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.BookingCancelledEvent as BookingCancelledOU
import qualified Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideCompletedEvent as OnUpdate
import qualified BecknV2.OnDemand.Types as Spec
import qualified Data.Aeson as A
import qualified Data.List as List
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.Vehicle as DVeh
import EulerHS.Prelude hiding (id, (%~))
import qualified Kernel.Types.Beckn.Gps as Gps
import Kernel.Types.Common
import Kernel.Utils.Common
import Tools.Error

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

mkRideCompletedPaymentType :: Maybe DMPM.PaymentMethodInfo -> Text
mkRideCompletedPaymentType = show . maybe OnUpdate.ON_FULFILLMENT (Common.castDPaymentType . (.paymentType))

showPaymentCollectedBy :: Maybe DMPM.PaymentMethodInfo -> Text
showPaymentCollectedBy = show . maybe OnUpdate.BPP (Common.castDPaymentCollector . (.collectedBy))

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

mkRideDistanceDetailsTags :: MonadFlow m => DRide.Ride -> m (Maybe [Spec.TagGroup])
mkRideDistanceDetailsTags ride = do
  chargeableDistance :: HighPrecMeters <-
    realToFrac <$> ride.chargeableDistance
      & fromMaybeM (InternalError "Ride chargeable distance is not present in OnUpdateBuildReq ride.")
  let traveledDistance :: HighPrecMeters = ride.traveledDistance
  pure $
    Just
      [ Spec.TagGroup
          { tagGroupDescriptor =
              Just $
                Spec.Descriptor
                  { descriptorCode = Just "ride_distance_details",
                    descriptorName = Just "Ride Distance Details",
                    descriptorShortDesc = Nothing
                  },
            tagGroupDisplay = Just False,
            tagGroupList =
              Just $
                chargeableDistanceSingleton chargeableDistance
                  ++ traveledDistanceSingleton traveledDistance
          }
      ]
  where
    chargeableDistanceSingleton chargeableDistance =
      List.singleton $
        Spec.Tag
          { tagDescriptor =
              Just $
                Spec.Descriptor
                  { descriptorCode = Just "chargeable_distance",
                    descriptorName = Just "Chargeable Distance",
                    descriptorShortDesc = Nothing
                  },
            tagDisplay = Just False,
            tagValue = Just $ show chargeableDistance
          }

    traveledDistanceSingleton traveledDistance =
      List.singleton $
        Spec.Tag
          { tagDescriptor =
              Just $
                Spec.Descriptor
                  { descriptorCode = Just "traveled_distance",
                    descriptorName = Just "Traveled Distance",
                    descriptorShortDesc = Nothing
                  },
            tagDisplay = Just False,
            tagValue = Just $ show traveledDistance
          }

mkDriverArrivedInfoTags :: Maybe UTCTime -> Maybe [Spec.TagGroup]
mkDriverArrivedInfoTags arrivalTime =
  Just
    [ Spec.TagGroup
        { tagGroupDescriptor =
            Just $
              Spec.Descriptor
                { descriptorCode = Just "driver_arrived_info",
                  descriptorName = Just "Driver Arrived Info",
                  descriptorShortDesc = Nothing
                },
          tagGroupDisplay = Just False,
          tagGroupList =
            Just $
              arrivalTimeSingleton
        }
    ]
  where
    arrivalTimeSingleton
      | isNothing arrivalTime = []
      | otherwise =
        List.singleton $
          Spec.Tag
            { tagDescriptor =
                Just $
                  Spec.Descriptor
                    { descriptorCode = Just "arrival_time",
                      descriptorName = Just "Chargeable Distance",
                      descriptorShortDesc = Nothing
                    },
              tagDisplay = Just False,
              tagValue = show <$> arrivalTime
            }

mkPreviousCancellationReasonsTags :: SBCR.CancellationSource -> Maybe [Spec.TagGroup]
mkPreviousCancellationReasonsTags cancellationSource =
  Just
    [ Spec.TagGroup
        { tagGroupDescriptor =
            Just $
              Spec.Descriptor
                { descriptorCode = Just "previous_cancellation_reasons",
                  descriptorName = Just "Previous Cancellation Reasons",
                  descriptorShortDesc = Nothing
                },
          tagGroupDisplay = Just False,
          tagGroupList =
            Just $
              cancellationSourceSingleton
        }
    ]
  where
    cancellationSourceSingleton =
      List.singleton $
        Spec.Tag
          { tagDescriptor =
              Just $
                Spec.Descriptor
                  { descriptorCode = Just "cancellation_reason",
                    descriptorName = Just "Chargeable Distance",
                    descriptorShortDesc = Nothing
                  },
            tagDisplay = Just False,
            tagValue = Just . show $ castCancellationSource cancellationSource
          }

castCancellationSource :: SBCR.CancellationSource -> BookingCancelledOU.CancellationSource
castCancellationSource = \case
  SBCR.ByUser -> BookingCancelledOU.ByUser
  SBCR.ByDriver -> BookingCancelledOU.ByDriver
  SBCR.ByMerchant -> BookingCancelledOU.ByMerchant
  SBCR.ByAllocator -> BookingCancelledOU.ByAllocator
  SBCR.ByApplication -> BookingCancelledOU.ByApplication

mkNewMessageTags :: Text -> Maybe [Spec.TagGroup]
mkNewMessageTags message =
  Just
    [ Spec.TagGroup
        { tagGroupDescriptor =
            Just $
              Spec.Descriptor
                { descriptorCode = Just "driver_new_message",
                  descriptorName = Just "Driver New Message",
                  descriptorShortDesc = Nothing
                },
          tagGroupDisplay = Just False,
          tagGroupList =
            Just $
              messageSingleton
        }
    ]
  where
    messageSingleton =
      List.singleton $
        Spec.Tag
          { tagDescriptor =
              Just $
                Spec.Descriptor
                  { descriptorCode = Just "message",
                    descriptorName = Just "New Message",
                    descriptorShortDesc = Nothing
                  },
            tagDisplay = Just False,
            tagValue = Just message
          }

mkSafetyAlertTags :: Text -> Text -> Maybe [Spec.TagGroup]
mkSafetyAlertTags reason code =
  Just
    [ Spec.TagGroup
        { tagGroupDescriptor =
            Just $
              Spec.Descriptor
                { descriptorCode = Just "safety_alert",
                  descriptorName = Just "Safety Alert",
                  descriptorShortDesc = Nothing
                },
          tagGroupDisplay = Just False,
          tagGroupList =
            Just $
              safetyAlertTriggerSingleton
        }
    ]
  where
    safetyAlertTriggerSingleton =
      List.singleton $
        Spec.Tag
          { tagDescriptor =
              Just $
                Spec.Descriptor
                  { descriptorCode = Just code,
                    descriptorName = Just "Safety Alert Trigger",
                    descriptorShortDesc = Nothing
                  },
            tagDisplay = Just False,
            tagValue = Just reason
          }
