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
import qualified BecknV2.OnDemand.Enums as EventEnum
import qualified BecknV2.OnDemand.Types as Spec
import qualified Data.List as L
import qualified Domain.Action.UI.Person as SP
import qualified Domain.Types.BecknConfig as DBC
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.FareParameters as DFParams
import qualified Domain.Types.FarePolicy as FarePolicyD
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Person as SP
import Domain.Types.Ride as DRide
import qualified Domain.Types.Vehicle as SVeh
import Kernel.External.Maps.Types as Maps
import Kernel.Prelude
import qualified Kernel.Types.Beckn.DecimalValue as DecimalValue
import Kernel.Types.Common
import Kernel.Utils.Common
import SharedLogic.Beckn.Common as Common
import qualified SharedLogic.FareCalculator as Fare
import Tools.Error

-- Identical for on_update and on_status
mkFulfillment ::
  (KvDbFlow m r, EncFlow m r) =>
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

buildRideCompletedQuote :: MonadFlow m => DRide.Ride -> DFParams.FareParameters -> m Quote.RideCompletedQuote
buildRideCompletedQuote ride fareParams = do
  fare <- ride.fare & fromMaybeM (InternalError "Ride fare is not present.")
  let value = Quote.DecimalValue $ fare.getHighPrecMoney
      currency = show ride.currency
  let price =
        Quote.QuotePrice
          { currency,
            value,
            computed_value = value
          }
      breakup =
        Fare.mkFareParamsBreakups (Breakup.BreakupItemPrice currency . DecimalValue.DecimalValue . getHighPrecMoney) Breakup.BreakupItem fareParams
          & filter (Common.filterRequiredBreakups $ DFParams.getFareParametersType fareParams) -- TODO: Remove after roll out
  pure
    Quote.RideCompletedQuote
      { price,
        breakup
      }

mkRideCompletedPayment :: Currency -> Maybe DMPM.PaymentMethodInfo -> Maybe Text -> Payment.Payment
mkRideCompletedPayment currency paymentMethodInfo paymentUrl = do
  Payment.Payment
    { _type = maybe Payment.ON_FULFILLMENT (Common.castDPaymentType . (.paymentType)) paymentMethodInfo,
      params =
        Payment.PaymentParams
          { collected_by = maybe Payment.BPP (Common.castDPaymentCollector . (.collectedBy)) paymentMethodInfo,
            instrument = Nothing,
            currency = show currency,
            amount = Nothing
          },
      uri = paymentUrl
    }

tfAssignedReqToOrder :: (MonadFlow m, EncFlow m r) => Common.DRideAssignedReq -> Maybe FarePolicyD.FullFarePolicy -> DBC.BecknConfig -> m Spec.Order
tfAssignedReqToOrder Common.DRideAssignedReq {..} mbFarePolicy becknConfig = do
  let Common.BookingDetails {..} = bookingDetails
      arrivalTimeTagGroup = if isValueAddNP then Utils.mkArrivalTimeTagGroupV2 ride.driverArrivalTime else Nothing
      quote = Utils.tfQuotation booking
      farePolicy = FarePolicyD.fullFarePolicyToFarePolicy <$> mbFarePolicy
      items = Utils.tfItems booking merchant.shortId.getShortId Nothing farePolicy Nothing
      payment = UtilsOU.mkPaymentParams paymentMethodInfo paymentUrl merchant bppConfig booking
  fulfillment <- Utils.mkFulfillmentV2 (Just driver) ride booking (Just vehicle) image arrivalTimeTagGroup Nothing isDriverBirthDay isFreeRide (Just $ show EventEnum.RIDE_ASSIGNED) isValueAddNP
  pure
    Spec.Order
      { orderId = Just $ booking.id.getId,
        orderStatus = Just $ show EventEnum.ACTIVE,
        orderFulfillments = Just [fulfillment],
        orderBilling = Nothing,
        orderCancellation = Nothing,
        orderCancellationTerms = Just $ Utils.tfCancellationTerms becknConfig,
        orderItems = items,
        orderPayments = Just [payment],
        orderProvider = Utils.tfProvider becknConfig,
        orderQuote = quote,
        orderCreatedAt = Just booking.createdAt,
        orderUpdatedAt = Just booking.updatedAt
      }

tfStartReqToOrder :: (MonadFlow m, EncFlow m r) => Common.DRideStartedReq -> Maybe FarePolicyD.FullFarePolicy -> DBC.BecknConfig -> m Spec.Order
tfStartReqToOrder Common.DRideStartedReq {..} mbFarePolicy becknConfig = do
  let Common.BookingDetails {..} = bookingDetails
      personTag = if isValueAddNP then Utils.mkLocationTagGroupV2 tripStartLocation else Nothing
      odometerTag = if isValueAddNP then Utils.mkOdometerTagGroupV2 ((.value) <$> ride.startOdometerReading) else Nothing
      arrivalTimeTagGroup = if isValueAddNP then Utils.mkArrivalTimeTagGroupV2 ride.driverArrivalTime else Nothing
      payment = UtilsOU.mkPaymentParams paymentMethodInfo paymentUrl merchant bppConfig booking
      quote = Utils.tfQuotation booking
      farePolicy = FarePolicyD.fullFarePolicyToFarePolicy <$> mbFarePolicy
      items = Utils.tfItems booking merchant.shortId.getShortId Nothing farePolicy Nothing
  fulfillment <- Utils.mkFulfillmentV2 (Just driver) ride booking (Just vehicle) Nothing (arrivalTimeTagGroup <> odometerTag) personTag False False (Just $ show EventEnum.RIDE_STARTED) isValueAddNP
  pure
    Spec.Order
      { orderId = Just $ booking.id.getId,
        orderStatus = Just $ show EventEnum.ACTIVE,
        orderFulfillments = Just [fulfillment],
        orderBilling = Nothing,
        orderCancellation = Nothing,
        orderCancellationTerms = Just $ Utils.tfCancellationTerms becknConfig,
        orderItems = items,
        orderPayments = Just [payment],
        orderProvider = Utils.tfProvider becknConfig,
        orderQuote = quote,
        orderCreatedAt = Just booking.createdAt,
        orderUpdatedAt = Just booking.updatedAt
      }

tfCompleteReqToOrder :: (MonadFlow m, EncFlow m r) => Common.DRideCompletedReq -> Maybe FarePolicyD.FullFarePolicy -> DBC.BecknConfig -> m Spec.Order
tfCompleteReqToOrder Common.DRideCompletedReq {..} mbFarePolicy becknConfig = do
  let Common.BookingDetails {..} = bookingDetails
  let personTag = if isValueAddNP then Utils.mkLocationTagGroupV2 tripEndLocation else Nothing
  let arrivalTimeTagGroup = if isValueAddNP then Utils.mkArrivalTimeTagGroupV2 ride.driverArrivalTime else Nothing
  distanceTagGroup <- if isValueAddNP then UtilsOU.mkDistanceTagGroup ride else return Nothing
  fulfillment <- Utils.mkFulfillmentV2 (Just driver) ride booking (Just vehicle) Nothing (arrivalTimeTagGroup <> distanceTagGroup) personTag False False (Just $ show EventEnum.RIDE_ENDED) isValueAddNP
  quote <- UtilsOU.mkRideCompletedQuote ride fareParams
  let farePolicy = FarePolicyD.fullFarePolicyToFarePolicy <$> mbFarePolicy
  let items = Utils.tfItems booking merchant.shortId.getShortId Nothing farePolicy Nothing
  let payment = UtilsOU.mkPaymentParams paymentMethodInfo paymentUrl merchant bppConfig booking
  pure $
    Spec.Order
      { orderId = Just $ booking.id.getId,
        orderStatus = Just $ show EventEnum.COMPLETE,
        orderFulfillments = Just [fulfillment],
        orderPayments = Just [payment],
        orderQuote = Just quote,
        orderBilling = Nothing,
        orderCancellation = Nothing,
        orderCancellationTerms = Just $ Utils.tfCancellationTerms becknConfig,
        orderItems = items,
        orderProvider = Utils.tfProvider becknConfig,
        orderCreatedAt = Just booking.createdAt,
        orderUpdatedAt = Just booking.updatedAt
      }

tfCancelReqToOrder :: (MonadFlow m, EncFlow m r) => Common.DBookingCancelledReq -> DBC.BecknConfig -> m Spec.Order
tfCancelReqToOrder Common.DBookingCancelledReq {..} becknConfig = do
  let quote = Utils.tfQuotation booking
  fulfillment <- forM bookingDetails $ \bookingDetails' -> do
    let Common.BookingDetails {driver, vehicle, ride, isValueAddNP} = bookingDetails'
    let image = Nothing
    let arrivalTimeTagGroup = if isValueAddNP then Utils.mkArrivalTimeTagGroupV2 ride.driverArrivalTime else Nothing
    Utils.mkFulfillmentV2 (Just driver) ride booking (Just vehicle) image arrivalTimeTagGroup Nothing False False (Just $ show EventEnum.RIDE_CANCELLED) isValueAddNP
  let payment = fmap (\bd -> L.singleton $ UtilsOU.mkPaymentParams Nothing Nothing bd.merchant becknConfig booking) bookingDetails
  pure
    Spec.Order
      { orderId = Just $ booking.id.getId,
        orderStatus = Just $ show EventEnum.CANCELLED,
        orderFulfillments = Just $ maybeToList fulfillment,
        orderCancellation =
          Just $
            Spec.Cancellation
              { cancellationCancelledBy = Just . show $ UtilsOU.castCancellationSource cancellationSource
              },
        orderBilling = Nothing,
        orderCancellationTerms = Just $ Utils.tfCancellationTerms becknConfig,
        orderItems = Nothing,
        orderPayments = payment,
        orderProvider = Utils.tfProvider becknConfig,
        orderQuote = quote,
        orderCreatedAt = Just booking.createdAt,
        orderUpdatedAt = Just booking.updatedAt
      }

tfArrivedReqToOrder :: (MonadFlow m, EncFlow m r) => Common.DDriverArrivedReq -> Maybe FarePolicyD.FullFarePolicy -> DBC.BecknConfig -> m Spec.Order
tfArrivedReqToOrder Common.DDriverArrivedReq {..} mbFarePolicy becknConfig = do
  let BookingDetails {..} = bookingDetails
      quote = Utils.tfQuotation booking
      payment = UtilsOU.mkPaymentParams paymentMethodInfo paymentUrl merchant bppConfig booking
      driverArrivedInfoTags = if isValueAddNP then Utils.mkArrivalTimeTagGroupV2 arrivalTime else Nothing
      farePolicy = FarePolicyD.fullFarePolicyToFarePolicy <$> mbFarePolicy
      items = Utils.tfItems booking merchant.shortId.getShortId Nothing farePolicy Nothing
  fulfillment <- Utils.mkFulfillmentV2 (Just driver) ride booking (Just vehicle) Nothing driverArrivedInfoTags Nothing False False (Just $ show EventEnum.RIDE_ARRIVED_PICKUP) isValueAddNP
  pure $
    Spec.Order
      { orderId = Just $ booking.id.getId,
        orderFulfillments = Just [fulfillment],
        orderBilling = Nothing,
        orderCancellation = Nothing,
        orderCancellationTerms = Just $ Utils.tfCancellationTerms becknConfig,
        orderItems = items,
        orderPayments = Just [payment],
        orderProvider = Utils.tfProvider becknConfig,
        orderQuote = quote,
        orderStatus = Just $ show EventEnum.ACTIVE,
        orderCreatedAt = Just booking.createdAt,
        orderUpdatedAt = Just booking.updatedAt
      }
