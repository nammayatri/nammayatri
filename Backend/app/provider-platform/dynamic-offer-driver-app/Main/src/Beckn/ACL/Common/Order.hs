{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.Common.Order
  ( mkFulfillment,
    buildDistanceTagGroup,
    mkOdometerTagGroup,
    mkArrivalTimeTagGroup,
    buildRideCompletedQuote,
    mkRideCompletedPayment,
    mkLocationTagGroup,
    tfAssignedReqToOrder,
    tfStartReqToOrder,
    tfCompleteReqToOrder,
    tfCancelReqToOrder,
    tfArrivedReqToOrder,
  )
where

import qualified Beckn.ACL.Common as Common
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified Beckn.OnDemand.Utils.OnUpdate as UtilsOU
import qualified Beckn.Types.Core.Taxi.Common.BreakupItem as Breakup
import qualified Beckn.Types.Core.Taxi.Common.FulfillmentInfo as RideFulfillment
import qualified Beckn.Types.Core.Taxi.Common.Payment as Payment
import qualified Beckn.Types.Core.Taxi.Common.RideCompletedQuote as Quote
import qualified Beckn.Types.Core.Taxi.Common.Tags as Tags
import qualified Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.OnUpdateEventType as Event
import qualified BecknV2.OnDemand.Types as Spec
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.FareParameters as DFParams
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Person as SP
import Domain.Types.Ride as DRide
import qualified Domain.Types.Vehicle as SVeh
import Kernel.External.Maps.Types as Maps
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Utils.Common
import SharedLogic.Beckn.Common as Common
import qualified SharedLogic.FareCalculator as Fare
import Tools.Error

-- Identical for on_update and on_status
mkFulfillment ::
  (EsqDBFlow m r, EncFlow m r) =>
  Maybe SP.Person ->
  DRide.Ride ->
  DRB.Booking ->
  Maybe SVeh.Vehicle ->
  Maybe Text ->
  Maybe Tags.TagGroups ->
  Maybe Tags.TagGroups ->
  Bool ->
  Bool ->
  m RideFulfillment.FulfillmentInfo
mkFulfillment mbDriver ride booking mbVehicle mbImage tags personTags isDriverBirthDay isFreeRide = do
  let rideOtp = fromMaybe ride.otp ride.endOtp
  agent <-
    forM mbDriver $ \driver -> do
      let agentTags =
            [ Tags.TagGroup
                { display = False,
                  code = "driver_details",
                  name = "Driver Details",
                  list =
                    [ Tags.Tag (Just False) (Just "registered_at") (Just "Registered At") (Just $ show driver.createdAt),
                      Tags.Tag (Just False) (Just "rating") (Just "rating") (show <$> driver.rating),
                      Tags.Tag (Just False) (Just "is_driver_birthday") (Just "Is Driver BirthDay") (Just $ show isDriverBirthDay),
                      Tags.Tag (Just False) (Just "is_free_ride") (Just "Is Free Ride") (Just $ show isFreeRide)
                    ]
                }
            ]
      mobileNumber <- SP.getPersonNumber driver >>= fromMaybeM (InternalError "Driver mobile number is not present.")
      name <- SP.getPersonFullName driver & fromMaybeM (PersonFieldNotPresent "firstName")
      pure $
        RideFulfillment.Agent
          { name = name,
            rateable = True,
            phone = Just mobileNumber,
            image = mbImage,
            tags = Just $ Tags.TG agentTags
          }
  let veh =
        mbVehicle <&> \vehicle ->
          RideFulfillment.Vehicle
            { model = vehicle.model,
              variant = show vehicle.variant,
              color = vehicle.color,
              registration = vehicle.registrationNo
            }
  let authorization =
        RideFulfillment.Authorization
          { _type = "OTP",
            token = rideOtp
          }
  let person =
        RideFulfillment.Person
          { tags = personTags
          }
  pure $
    RideFulfillment.FulfillmentInfo
      { id = ride.id.getId,
        start =
          RideFulfillment.StartInfo
            { authorization = Just authorization, -- TODO :: Remove authorization for NormalBooking once Customer side code is decoupled.
              location =
                RideFulfillment.Location
                  { gps = RideFulfillment.Gps {lat = booking.fromLocation.lat, lon = booking.fromLocation.lon}
                  },
              time = ride.tripStartTime <&> \tripStartTime -> RideFulfillment.TimeTimestamp {timestamp = tripStartTime}
            },
        end =
          ( \toLocation ->
              RideFulfillment.EndInfo
                { location =
                    RideFulfillment.Location
                      { gps = RideFulfillment.Gps {lat = toLocation.lat, lon = toLocation.lon} -- assuming locations will always be in correct order in list
                      },
                  time = ride.tripEndTime <&> \tripEndTime -> RideFulfillment.TimeTimestamp {timestamp = tripEndTime}
                }
          )
            <$> booking.toLocation,
        agent,
        _type = Common.mkFulfillmentType booking.tripCategory,
        vehicle = veh,
        ..
      }

buildDistanceTagGroup :: MonadFlow m => DRide.Ride -> m [Tags.TagGroup]
buildDistanceTagGroup ride = do
  chargeableDistance :: HighPrecMeters <-
    realToFrac <$> ride.chargeableDistance
      & fromMaybeM (InternalError "Ride chargeable distance is not present.")
  let traveledDistance :: HighPrecMeters = ride.traveledDistance
      endOdometerValue = (.value) <$> ride.endOdometerReading
  pure
    [ Tags.TagGroup
        { display = False,
          code = "ride_distance_details",
          name = "Ride Distance Details",
          list =
            [ Tags.Tag (Just False) (Just "chargeable_distance") (Just "Chargeable Distance") (Just $ show chargeableDistance),
              Tags.Tag (Just False) (Just "traveled_distance") (Just "Traveled Distance") (Just $ show traveledDistance)
            ]
              <> [Tags.Tag (Just False) (Just "end_odometer_reading") (Just "End Odometer Reading") (show <$> endOdometerValue) | isJust endOdometerValue]
        }
    ]

mkOdometerTagGroup :: Maybe Centesimal -> [Tags.TagGroup]
mkOdometerTagGroup startOdometerReading =
  [ Tags.TagGroup
      { display = False,
        code = "ride_odometer_details",
        name = "Ride Odometer Details",
        list = [Tags.Tag (Just False) (Just "start_odometer_reading") (Just "Start Odometer Reading") (show <$> startOdometerReading) | isJust startOdometerReading]
      }
  ]

mkLocationTagGroup :: Maybe Maps.LatLong -> [Tags.TagGroup]
mkLocationTagGroup location =
  [ Tags.TagGroup
      { display = False,
        code = "current_location",
        name = "Current Location",
        list =
          [ Tags.Tag (Just False) (Just "current_location_lat") (Just "Current Location Lat") ((\loc -> Just $ show loc.lat) =<< location),
            Tags.Tag (Just False) (Just "current_location_lon") (Just "Current Location Lon") ((\loc -> Just $ show loc.lon) =<< location)
          ]
      }
  ]

mkArrivalTimeTagGroup :: Maybe UTCTime -> [Tags.TagGroup]
mkArrivalTimeTagGroup arrivalTime =
  [ Tags.TagGroup
      { display = False,
        code = "driver_arrived_info",
        name = "Driver Arrived Info",
        list = [Tags.Tag (Just False) (Just "arrival_time") (Just "Chargeable Distance") (show <$> arrivalTime) | isJust arrivalTime]
      }
  ]

currency :: Text
currency = "INR"

buildRideCompletedQuote :: MonadFlow m => DRide.Ride -> DFParams.FareParameters -> m Quote.RideCompletedQuote
buildRideCompletedQuote ride fareParams = do
  fare <- realToFrac <$> ride.fare & fromMaybeM (InternalError "Ride fare is not present.")
  let price =
        Quote.QuotePrice
          { currency,
            value = fare,
            computed_value = fare
          }
      breakup =
        Fare.mkFareParamsBreakups (Breakup.BreakupItemPrice currency . fromIntegral) Breakup.BreakupItem fareParams
          & filter (Common.filterRequiredBreakups $ DFParams.getFareParametersType fareParams) -- TODO: Remove after roll out
  pure
    Quote.RideCompletedQuote
      { price,
        breakup
      }

mkRideCompletedPayment :: Maybe DMPM.PaymentMethodInfo -> Maybe Text -> Payment.Payment
mkRideCompletedPayment paymentMethodInfo paymentUrl = do
  Payment.Payment
    { _type = maybe Payment.ON_FULFILLMENT (Common.castDPaymentType . (.paymentType)) paymentMethodInfo,
      params =
        Payment.PaymentParams
          { collected_by = maybe Payment.BPP (Common.castDPaymentCollector . (.collectedBy)) paymentMethodInfo,
            instrument = Nothing,
            currency,
            amount = Nothing
          },
      uri = paymentUrl
    }

tfAssignedReqToOrder :: (MonadFlow m, EncFlow m r) => Common.DRideAssignedReq -> m Spec.Order
tfAssignedReqToOrder Common.DRideAssignedReq {..} = do
  let Common.BookingDetails {driver, vehicle, ride, booking} = bookingDetails
  let arrivalTimeTagGroup = Utils.mkArrivalTimeTagGroupV2 ride.driverArrivalTime
  fulfillment <- Utils.mkFulfillmentV2 (Just driver) ride booking (Just vehicle) image (Just arrivalTimeTagGroup) Nothing isDriverBirthDay isFreeRide (Just $ show Event.RIDE_ASSIGNED)
  pure
    Spec.Order
      { orderId = Just $ booking.id.getId,
        orderStatus = Just "ACTIVE",
        orderFulfillments = Just [fulfillment],
        orderBilling = Nothing,
        orderCancellation = Nothing,
        orderCancellationTerms = Nothing,
        orderItems = Nothing,
        orderPayments = Nothing,
        orderProvider = Nothing,
        orderQuote = Nothing
      }

tfStartReqToOrder :: (MonadFlow m, EncFlow m r) => Common.DRideStartedReq -> m Spec.Order
tfStartReqToOrder Common.DRideStartedReq {..} = do
  let Common.BookingDetails {driver, vehicle, ride, booking} = bookingDetails
  let personTag = Utils.mkLocationTagGroupV2 tripStartLocation -- why are we sending trip start and end location in personTags?
      odometerTag = Utils.mkOdometerTagGroupV2 ((.value) <$> ride.startOdometerReading)
  let arrivalTimeTagGroup = Utils.mkArrivalTimeTagGroupV2 ride.driverArrivalTime
  fulfillment <- Utils.mkFulfillmentV2 (Just driver) ride booking (Just vehicle) Nothing (Just arrivalTimeTagGroup <> Just odometerTag) (Just personTag) False False (Just $ show Event.RIDE_STARTED)
  pure
    Spec.Order
      { orderId = Just $ booking.id.getId,
        orderStatus = Just "ACTIVE",
        orderFulfillments = Just [fulfillment],
        orderBilling = Nothing,
        orderCancellation = Nothing,
        orderCancellationTerms = Nothing,
        orderItems = Nothing,
        orderPayments = Nothing,
        orderProvider = Nothing,
        orderQuote = Nothing
      }

tfCompleteReqToOrder :: (MonadFlow m, EncFlow m r) => Common.DRideCompletedReq -> m Spec.Order
tfCompleteReqToOrder Common.DRideCompletedReq {..} = do
  let Common.BookingDetails {driver, vehicle, ride, booking} = bookingDetails
  let personTag = Utils.mkLocationTagGroupV2 tripEndLocation
  let arrivalTimeTagGroup = Utils.mkArrivalTimeTagGroupV2 ride.driverArrivalTime
  distanceTagGroup <- UtilsOU.mkDistanceTagGroup ride
  fulfillment <- Utils.mkFulfillmentV2 (Just driver) ride booking (Just vehicle) Nothing (Just arrivalTimeTagGroup <> distanceTagGroup) (Just personTag) False False (Just $ show Event.RIDE_ENDED)
  quote <- UtilsOU.mkRideCompletedQuote ride fareParams
  pure
    Spec.Order
      { orderId = Just $ booking.id.getId,
        orderStatus = Just "COMPLETE",
        orderFulfillments = Just [fulfillment],
        orderPayments = Just $ UtilsOU.mkRideCompletedPayment paymentMethodInfo paymentUrl,
        orderQuote = Just quote,
        orderBilling = Nothing,
        orderCancellation = Nothing,
        orderCancellationTerms = Nothing,
        orderItems = Nothing,
        orderProvider = Nothing
      }

tfCancelReqToOrder :: (MonadFlow m, EncFlow m r) => Common.DBookingCancelledReq -> m Spec.Order
tfCancelReqToOrder Common.DBookingCancelledReq {..} = do
  fulfillment <- forM bookingDetails $ \bookingDetails' -> do
    let Common.BookingDetails {driver, vehicle, ride} = bookingDetails'
    let image = Nothing
    let arrivalTimeTagGroup = Utils.mkArrivalTimeTagGroupV2 ride.driverArrivalTime
    Utils.mkFulfillmentV2 (Just driver) ride booking (Just vehicle) image (Just arrivalTimeTagGroup) Nothing False False (Just $ show Event.RIDE_CANCELLED)
  pure
    Spec.Order
      { orderId = Just $ booking.id.getId,
        orderStatus = Just "CANCELLED",
        orderFulfillments = Just $ maybeToList fulfillment,
        orderCancellation =
          Just $
            Spec.Cancellation
              { cancellationCancelledBy = Just . show $ UtilsOU.castCancellationSource cancellationSource
              },
        orderBilling = Nothing,
        orderCancellationTerms = Nothing,
        orderItems = Nothing,
        orderPayments = Nothing,
        orderProvider = Nothing,
        orderQuote = Nothing
      }

tfArrivedReqToOrder :: (MonadFlow m, EncFlow m r) => Common.DDriverArrivedReq -> m Spec.Order
tfArrivedReqToOrder Common.DDriverArrivedReq {..} = do
  let BookingDetails {..} = bookingDetails
  let driverArrivedInfoTags = Utils.mkArrivalTimeTagGroupV2 arrivalTime
  fulfillment <- Utils.mkFulfillmentV2 (Just driver) ride booking (Just vehicle) Nothing (Just driverArrivedInfoTags) Nothing False False (Just $ show Event.RIDE_ARRIVED_PICKUP)
  pure $
    Spec.Order
      { orderId = Just $ booking.id.getId,
        orderFulfillments = Just [fulfillment],
        orderBilling = Nothing,
        orderCancellation = Nothing,
        orderCancellationTerms = Nothing,
        orderItems = Nothing,
        orderPayments = Nothing,
        orderProvider = Nothing,
        orderQuote = Nothing,
        orderStatus = Just "ACTIVE"
      }
