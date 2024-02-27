{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnUpdate
  ( buildOnUpdateMessage,
    buildOnUpdateMessageV2,
    module Reexport,
  )
where

import qualified Beckn.ACL.Common as Common
import qualified Beckn.ACL.Common.Order as Common
import qualified Beckn.OnDemand.Transformer.OnUpdate as TFOU
import qualified Beckn.OnDemand.Utils.Common as BUtils
import qualified Beckn.OnDemand.Utils.OnUpdate as Utils
import qualified Beckn.Types.Core.Taxi.Common.Tags as Tags
import qualified Beckn.Types.Core.Taxi.OnUpdate as OnUpdate
import qualified Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.BookingCancelledEvent as BookingCancelledOU
import qualified Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.DriverArrivedEvent as DriverArrivedOU
import qualified Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.EstimateRepetitionEvent as EstimateRepetitionOU
import qualified Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.NewMessageEvent as NewMessageOU
import qualified Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideAssignedEvent as RideAssignedOU
import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideCompletedEvent as OnUpdate
import qualified Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideCompletedEvent as RideCompletedOU
import qualified Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideStartedEvent as RideStartedOU
import qualified Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.SafetyAlertEvent as SafetyAlertDU
import qualified Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.StopArrivedEvent as StopArrivedOU
import qualified Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.UpdatedEstimateEvent as UpdatedEstimateOU
import Beckn.Types.Core.Taxi.Search.StartInfo as SI
import Beckn.Types.Core.Taxi.Search.StopInfo as SI
import qualified BecknV2.OnDemand.Types as Spec
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Merchant as DM
import Domain.Types.OnUpdate as Reexport
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.FareCalculator
import SharedLogic.FarePolicy
import qualified Storage.CachedQueries.BecknConfig as QBC

buildOnUpdateMessage ::
  (EsqDBFlow m r, EncFlow m r) =>
  OnUpdateBuildReq ->
  m OnUpdate.OnUpdateMessage
buildOnUpdateMessage (RideAssignedBuildReq DRideAssignedReq {..}) = do
  let BookingDetails {..} = bookingDetails
  fulfillment <- Common.mkFulfillment (Just driver) ride booking (Just vehicle) image Nothing Nothing isDriverBirthDay isFreeRide
  return $
    OnUpdate.OnUpdateMessage
      { order =
          OnUpdate.RideAssigned $
            RideAssignedOU.RideAssignedEvent
              { id = booking.id.getId,
                state = "ACTIVE",
                ..
              },
        update_target = "order.fufillment.state.code, order.fulfillment.agent, order.fulfillment.vehicle" <> ", order.fulfillment.start.authorization" -- TODO :: Remove authorization for NormalBooking once Customer side code is decoupled.
      }
buildOnUpdateMessage (RideStartedBuildReq DRideStartedReq {..}) = do
  let BookingDetails {..} = bookingDetails
  let personTag = Common.mkLocationTagGroup tripStartLocation
      odometerTag = Common.mkOdometerTagGroup ((.value) <$> ride.startOdometerReading)
  fulfillment <- Common.mkFulfillment (Just driver) ride booking (Just vehicle) Nothing (Just $ Tags.TG odometerTag) (Just $ Tags.TG personTag) False False
  return $
    OnUpdate.OnUpdateMessage
      { order =
          OnUpdate.RideStarted $
            RideStartedOU.RideStartedEvent
              { id = booking.id.getId,
                ..
              },
        update_target = "order.fufillment.state.code"
      }
buildOnUpdateMessage (RideCompletedBuildReq DRideCompletedReq {..}) = do
  let BookingDetails {..} = bookingDetails
  let personTag = Common.mkLocationTagGroup tripEndLocation
  distanceTagGroup <- Common.buildDistanceTagGroup ride
  fulfillment <- Common.mkFulfillment (Just driver) ride booking (Just vehicle) Nothing (Just $ Tags.TG distanceTagGroup) (Just $ Tags.TG personTag) False False
  quote <- Common.buildRideCompletedQuote ride fareParams
  return $
    OnUpdate.OnUpdateMessage
      { order =
          OnUpdate.RideCompleted
            RideCompletedOU.RideCompletedEvent
              { id = booking.id.getId,
                quote,
                payment = Just $ Common.mkRideCompletedPayment paymentMethodInfo paymentUrl,
                fulfillment = fulfillment
              },
        update_target = "order.payment, order.quote, order.fulfillment.tags, order.fulfillment.state.tags"
      }
buildOnUpdateMessage (BookingCancelledBuildReq DBookingCancelledReq {..}) = do
  return $
    OnUpdate.OnUpdateMessage
      { order =
          OnUpdate.BookingCancelled $
            BookingCancelledOU.BookingCancelledEvent
              { id = booking.id.getId,
                state = "CANCELLED",
                cancellation_reason = Common.castCancellationSource cancellationSource
              },
        update_target = "state,fufillment.state.code"
      }
buildOnUpdateMessage (DriverArrivedBuildReq DDriverArrivedReq {..}) = do
  let BookingDetails {..} = bookingDetails
  let tagGroups = Common.mkArrivalTimeTagGroup arrivalTime
  fulfillment <- Common.mkFulfillment (Just driver) ride booking (Just vehicle) Nothing (Just $ Tags.TG tagGroups) Nothing False False
  return $
    OnUpdate.OnUpdateMessage
      { order =
          OnUpdate.DriverArrived $
            DriverArrivedOU.DriverArrivedEvent
              { id = ride.bookingId.getId,
                fulfillment
              },
        update_target = "order.fufillment.state.code, order.fulfillment.tags"
      }
buildOnUpdateMessage (EstimateRepetitionBuildReq DEstimateRepetitionReq {..}) = do
  let BookingDetails {..} = bookingDetails
  let tagGroups =
        [ Tags.TagGroup
            { display = False,
              code = "previous_cancellation_reasons",
              name = "Previous Cancellation Reasons",
              list = [Tags.Tag (Just False) (Just "cancellation_reason") (Just "Chargeable Distance") (Just . show $ Common.castCancellationSource cancellationSource)]
            }
        ]
  fulfillment <- Common.mkFulfillment Nothing ride booking Nothing Nothing (Just $ Tags.TG tagGroups) Nothing False False
  let item = EstimateRepetitionOU.Item {id = estimateId.getId}
  return $
    OnUpdate.OnUpdateMessage
      { order =
          OnUpdate.EstimateRepetition $
            EstimateRepetitionOU.EstimateRepetitionEvent
              { id = booking.id.getId,
                item = item,
                fulfillment
              },
        update_target = "order.fufillment.state.code, order.tags"
      }
buildOnUpdateMessage (NewMessageBuildReq DNewMessageReq {..}) = do
  let BookingDetails {..} = bookingDetails
  let tagGroups =
        [ Tags.TagGroup
            { display = False,
              code = "driver_new_message",
              name = "Driver New Message",
              list = [Tags.Tag (Just False) (Just "message") (Just "New Message") (Just message)]
            }
        ]
  fulfillment <- Common.mkFulfillment (Just driver) ride booking (Just vehicle) Nothing (Just $ Tags.TG tagGroups) Nothing False False
  return $
    OnUpdate.OnUpdateMessage
      { update_target = "order.fufillment.state.code, order.fulfillment.tags",
        order =
          OnUpdate.NewMessage $
            NewMessageOU.NewMessageEvent
              { id = ride.bookingId.getId,
                fulfillment = fulfillment
              }
      }
buildOnUpdateMessage (SafetyAlertBuildReq DSafetyAlertReq {..}) = do
  let BookingDetails {..} = bookingDetails
  let tagGroups =
        [ Tags.TagGroup
            { display = False,
              code = "safety_alert",
              name = "Safety Alert",
              list = [Tags.Tag (Just False) (Just code) (Just "Safety Alert Trigger") (Just reason)]
            }
        ]
  fulfillment <- Common.mkFulfillment Nothing ride booking Nothing Nothing (Just $ Tags.TG tagGroups) Nothing False False
  return $
    OnUpdate.OnUpdateMessage
      { order =
          OnUpdate.SafetyAlert $
            SafetyAlertDU.SafetyAlertEvent
              { id = ride.bookingId.getId,
                fulfillment = fulfillment
              },
        update_target = "order.fufillment.state.code, order.fulfillment.tags"
      }
buildOnUpdateMessage (StopArrivedBuildReq DStopArrivedBuildReq {..}) = do
  let BookingDetails {..} = bookingDetails
  fulfillment <- Common.mkFulfillment Nothing ride booking Nothing Nothing Nothing Nothing False False
  return $
    OnUpdate.OnUpdateMessage
      { order =
          OnUpdate.StopArrived $
            StopArrivedOU.StopArrivedEvent
              { id = booking.id.getId,
                ..
              },
        update_target = "order.fufillment.state.code"
      }
buildOnUpdateMessage (UpdatedEstimateBuildReq res@DUpdatedEstimateReq {..}) = do
  let BookingDetails {..} = bookingDetails
  let tagGroups =
        [ Tags.TagGroup
            { display = False,
              code = "updated_estimate",
              name = "Updated Estimate",
              list = [Tags.Tag (Just False) (Just "updated_estimate") (Just "Updated Estimate") (Just "true")]
            }
        ]
  fulfillmentOld <- Common.mkFulfillment Nothing ride booking Nothing Nothing (Just $ Tags.TG tagGroups) Nothing False False
  let startInfo = mkStartInfo res
  let stopInfo = mkStopInfo res
  let pricings = [Utils.convertEstimateToPricing estimateRevised, Utils.convertQuoteToPricing quoteRevised]
  let (pricingEntities :: [PricingEntities]) = map (mkPricingEntities startInfo stopInfo provider) pricings
  let item = map (.item) pricingEntities
      fulfillment = map (.fulfillment) pricingEntities
  let providerSpec =
        OnUpdate.Provider
          { id = provider.subscriberId.getShortId,
            descriptor = OnUpdate.Descriptor {name = provider.name},
            item,
            fulfillment
          }
  return $
    OnUpdate.OnUpdateMessage
      { order =
          OnUpdate.UpdatedEstimate $
            UpdatedEstimateOU.UpdatedEstimateEvent
              { id = booking.id.getId,
                bpp_providers = pure providerSpec,
                bpp_descriptor = OnUpdate.Descriptor provider.name,
                fulfillment = fulfillmentOld
              },
        update_target = "order.fufillment.state.code, order.fulfillment.tags"
      }

buildOnUpdateMessageV2 ::
  ( MonadFlow m,
    EsqDBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CacheFlow m r
  ) =>
  DM.Merchant ->
  DRB.Booking ->
  OnUpdateBuildReq ->
  m Spec.OnUpdateReq
buildOnUpdateMessageV2 merchant booking req = do
  msgId <- generateGUID
  let bppId = getShortId $ merchant.subscriberId
      city = fromMaybe merchant.city booking.bapCity
      country = fromMaybe merchant.country booking.bapCountry
  bppUri <- BUtils.mkBppUri merchant.id.getId
  bppConfig <- QBC.findByMerchantIdDomainAndVehicle merchant.id "MOBILITY" (BUtils.mapVariantToVehicle booking.vehicleVariant) >>= fromMaybeM (InternalError "Beckn Config not found")
  TFOU.buildOnUpdateReqV2 Context.ON_UPDATE Context.MOBILITY msgId bppId bppUri city country booking req bppConfig merchant

mkStartInfo :: DUpdatedEstimateReq -> SI.StartInfo
mkStartInfo dReq =
  SI.StartInfo
    { location =
        OnUpdate.Location
          { gps = OnUpdate.Gps {lat = dReq.fromLocation.lat, lon = dReq.fromLocation.lon},
            address = Nothing
          }
    }

mkStopInfo :: DUpdatedEstimateReq -> Maybe SI.StopInfo
mkStopInfo res =
  ( \toLoc ->
      SI.StopInfo
        { location =
            OnUpdate.Location
              { gps = OnUpdate.Gps {lat = toLoc.lat, lon = toLoc.lon},
                address = Nothing
              }
        }
  )
    <$> res.toLocation

data PricingEntities = PricingEntities
  { fulfillment :: OnUpdate.FulfillmentInfo,
    item :: OnUpdate.Item
  }

currency' :: Text
currency' = "INR"

mkPricingEntities :: SI.StartInfo -> Maybe SI.StopInfo -> DM.Merchant -> Utils.Pricing -> PricingEntities
mkPricingEntities start end provider pricing = do
  let variant = Common.castVariant pricing.vehicleVariant
      minPriceDecimalValue = OnUpdate.DecimalValue $ toRational pricing.pricingMinFare
      maxPriceDecimalValue = OnUpdate.DecimalValue $ toRational pricing.pricingMaxFare
      fareParamsBreakups = maybe [] (mkFareParamsBreakups Utils.mkPriceHere Utils.mkFareParamsBreakupItem) pricing.fareParams
      fareParamsBreakupsTags = buildFareParamsBreakupsTags <$> fareParamsBreakups

      rateCardBreakups = maybe [] (mkFarePolicyBreakups Utils.mkValue Utils.mkRateCardBreakupItem pricing.estimatedDistance) pricing.farePolicy
      rateCardTags = buildRateCardTags <$> rateCardBreakups

      fulfillment =
        OnUpdate.FulfillmentInfo
          { start,
            end = end,
            id = pricing.pricingId,
            _type = pricing.fulfillmentType,
            vehicle = OnUpdate.Vehicle {category = variant}
          }
      item =
        OnUpdate.Item
          { id = Common.mkItemId provider.shortId.getShortId pricing.vehicleVariant,
            fulfillment_id = fulfillment.id,
            price =
              OnUpdate.ItemPrice
                { currency = currency',
                  value = minPriceDecimalValue,
                  offered_value = minPriceDecimalValue,
                  minimum_value = minPriceDecimalValue,
                  maximum_value = maxPriceDecimalValue
                },
            tags =
              Just $
                OnUpdate.TG
                  [ mkGeneralInfoTag,
                    mkFareParamsTag fareParamsBreakupsTags,
                    mkRateCardTag rateCardTags
                  ]
          }
  PricingEntities
    { fulfillment,
      item
    }
  where
    mkGeneralInfoTag =
      let specialLocationTag = pricing.specialLocationTag
       in OnUpdate.TagGroup
            { display = False,
              code = "general_info",
              name = "General Information",
              list =
                [ OnUpdate.Tag
                    { display = (\_ -> Just True) =<< specialLocationTag,
                      code = (\_ -> Just "special_location_tag") =<< specialLocationTag,
                      name = (\_ -> Just "Special Location Tag") =<< specialLocationTag,
                      value = specialLocationTag
                    },
                  OnUpdate.Tag
                    { display = Just False,
                      code = Just "distance_to_nearest_driver",
                      name = Just "Distance To Nearest Driver",
                      value = Nothing
                    }
                ]
            }

    mkFareParamsTag fareParamsBreakupsTags =
      OnUpdate.TagGroup
        { display = False,
          code = "fare_breakup",
          name = "Fare Breakup",
          list = fareParamsBreakupsTags
        }

    mkRateCardTag rateCardTags =
      OnUpdate.TagGroup
        { display = False,
          code = "rate_card",
          name = "Rate Card",
          list = rateCardTags
        }

buildFareParamsBreakupsTags ::
  Utils.FareParamsBreakupItem ->
  OnUpdate.Tag
buildFareParamsBreakupsTags Utils.FareParamsBreakupItem {..} = do
  OnUpdate.Tag
    { display = Just False,
      code = Just title,
      name = Just title,
      value = Just $ show price.getMoney
    }

buildRateCardTags ::
  Utils.RateCardBreakupItem ->
  OnUpdate.Tag
buildRateCardTags Utils.RateCardBreakupItem {..} = do
  OnUpdate.Tag
    { display = Just False,
      code = Just title,
      name = Just title,
      value = Just value
    }
