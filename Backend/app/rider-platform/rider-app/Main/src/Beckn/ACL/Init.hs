{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}

module Beckn.ACL.Init (buildInitReq, buildInitReqV2) where

import qualified Beckn.ACL.Common as Common
import qualified Beckn.Types.Core.Taxi.Init as Init
import Control.Lens ((%~))
import qualified Data.Text as T
import qualified Domain.Types.Location as DL
import qualified Domain.Types.LocationAddress as DLA
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import qualified Domain.Types.VehicleVariant as VehVar
-- import Environment
-- import Kernel.External.Maps.Types (LatLong)
import Kernel.Prelude
import Kernel.Types.App
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Beckn.ReqTypes
import Kernel.Types.Logging
import Kernel.Utils.Common
import qualified SharedLogic.Confirm as SConfirm

buildInitReq ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl]) =>
  SConfirm.DConfirmRes ->
  m (BecknReq Init.InitMessage)
buildInitReq res = do
  let transactionId = res.searchRequestId.getId
  bapUrl <- asks (.nwAddress) <&> #baseUrlPath %~ (<> "/" <> T.unpack res.merchant.id.getId)
  -- TODO :: Add request city, after multiple city support on gateway.
  context <- buildTaxiContext Context.INIT res.booking.id.getId (Just transactionId) res.merchant.bapId bapUrl (Just res.providerId) (Just res.providerUrl) res.merchant.defaultCity res.merchant.country False
  initMessage <- buildInitMessage res
  pure $ BecknReq context initMessage

buildInitReqV2 ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl]) =>
  SConfirm.DConfirmRes ->
  m (BecknReqV2 Init.InitMessageV2)
buildInitReqV2 res = do
  let transactionId = res.searchRequestId.getId
  bapUrl <- asks (.nwAddress) <&> #baseUrlPath %~ (<> "/" <> T.unpack res.merchant.id.getId)
  -- TODO :: Add request city, after multiple city support on gateway.
  context <- buildTaxiContextV2 Context.INIT res.booking.id.getId (Just transactionId) res.merchant.bapId bapUrl (Just res.providerId) (Just res.providerUrl) res.merchant.defaultCity res.merchant.country
  initMessage <- buildInitMessageV2 res
  pure $ BecknReqV2 context initMessage

buildInitMessage :: (MonadThrow m, Log m) => SConfirm.DConfirmRes -> m Init.InitMessage
buildInitMessage res = do
  let (fulfillmentType, mbBppFullfillmentId, mbDriverId) = case res.quoteDetails of
        SConfirm.ConfirmOneWayDetails -> (Init.RIDE, Nothing, Nothing)
        SConfirm.ConfirmRentalDetails _ -> (Init.RIDE, Nothing, Nothing)
        SConfirm.ConfirmAutoDetails estimateId driverId -> (Init.RIDE, Just estimateId, driverId)
        SConfirm.ConfirmOneWaySpecialZoneDetails quoteId -> (Init.RIDE_OTP, Just quoteId, Nothing) --need to be  checked
  let vehicleVariant = castVehicleVariant res.vehicleVariant
  pure
    Init.InitMessage
      { order =
          Init.Order
            { items = [mkOrderItem res.itemId mbBppFullfillmentId],
              quote =
                Init.Quote
                  { price =
                      Init.QuotePrice
                        { value = fromIntegral res.booking.estimatedFare,
                          offered_value = fromIntegral res.booking.estimatedTotalFare,
                          currency = "INR"
                        },
                    breakup = Nothing
                  },
              billing = mkBilling res.riderPhone res.riderName,
              fulfillment = mkFulfillmentInfo fulfillmentType mbBppFullfillmentId res.fromLoc res.toLoc res.maxEstimatedDistance vehicleVariant,
              payment = mkPayment res.paymentMethodInfo,
              provider = mkProvider mbDriverId
            }
      }
  where
    castVehicleVariant = \case
      VehVar.SEDAN -> Init.SEDAN
      VehVar.SUV -> Init.SUV
      VehVar.HATCHBACK -> Init.HATCHBACK
      VehVar.AUTO_RICKSHAW -> Init.AUTO_RICKSHAW
      VehVar.TAXI -> Init.TAXI
      VehVar.TAXI_PLUS -> Init.TAXI_PLUS

buildInitMessageV2 :: (MonadThrow m, Log m) => SConfirm.DConfirmRes -> m Init.InitMessageV2
buildInitMessageV2 res = do
  let (fulfillmentType, mbBppFullfillmentId, mbDriverId) = case res.quoteDetails of
        SConfirm.ConfirmOneWayDetails -> (Init.RIDE, Nothing, Nothing)
        SConfirm.ConfirmRentalDetails _ -> (Init.RIDE, Nothing, Nothing)
        SConfirm.ConfirmAutoDetails estimateId driverId -> (Init.RIDE, Just estimateId, driverId)
        SConfirm.ConfirmOneWaySpecialZoneDetails quoteId -> (Init.RIDE_OTP, Just quoteId, Nothing) --need to be  checked
  let vehicleVariant = castVehicleVariant res.vehicleVariant
  pure
    Init.InitMessageV2
      { order =
          Init.OrderV2
            { items = [mkOrderItemV2 res.itemId mbBppFullfillmentId],
              quote =
                Init.Quote
                  { price =
                      Init.QuotePrice
                        { value = fromIntegral res.booking.estimatedFare,
                          offered_value = fromIntegral res.booking.estimatedTotalFare,
                          currency = "INR"
                        },
                    breakup = Nothing
                  },
              billing = mkBilling res.riderPhone res.riderName,
              fulfillments = [mkFulfillmentInfoV2 fulfillmentType mbBppFullfillmentId res.fromLoc res.toLoc res.maxEstimatedDistance vehicleVariant],
              payments = [mkPaymentV2 res.paymentMethodInfo],
              provider = mkProvider mbDriverId
            }
      }
  where
    castVehicleVariant = \case
      VehVar.SEDAN -> Init.SEDAN
      VehVar.SUV -> Init.SUV
      VehVar.HATCHBACK -> Init.HATCHBACK
      VehVar.AUTO_RICKSHAW -> Init.AUTO_RICKSHAW
      VehVar.TAXI -> Init.TAXI
      VehVar.TAXI_PLUS -> Init.TAXI_PLUS

mkBilling :: Maybe Text -> Maybe Text -> Init.Billing
mkBilling phone name = Init.Billing {..}

mkProvider :: Maybe Text -> Maybe Init.Provider
mkProvider driverId =
  driverId >>= \dId ->
    Just
      Init.Provider
        { id = dId
        }

mkOrderItem :: Text -> Maybe Text -> Init.OrderItem
mkOrderItem itemId mbBppFullfillmentId =
  Init.OrderItem
    { id = itemId,
      fulfillment_id = mbBppFullfillmentId
    }

mkOrderItemV2 :: Text -> Maybe Text -> Init.OrderItemV2
mkOrderItemV2 itemId mbBppFullfillmentId =
  let fulfillmentIds = maybeToList mbBppFullfillmentId
   in Init.OrderItemV2
        { id = itemId,
          fulfillment_ids = Just fulfillmentIds
        }

mkFulfillmentInfo :: Init.FulfillmentType -> Maybe Text -> DL.Location -> Maybe DL.Location -> Maybe HighPrecMeters -> Init.VehicleVariant -> Init.FulfillmentInfo
mkFulfillmentInfo fulfillmentType mbBppFullfillmentId fromLoc mbToLoc mbMaxDistance vehicleVariant =
  Init.FulfillmentInfo
    { id = mbBppFullfillmentId,
      _type = fulfillmentType,
      tags =
        if isJust mbMaxDistance
          then
            Just $
              Init.TG
                [ Init.TagGroup
                    { display = True,
                      code = "estimations",
                      name = "Estimations",
                      list =
                        [ Init.Tag
                            { display = (\_ -> Just True) =<< mbMaxDistance,
                              code = (\_ -> Just "max_estimated_distance") =<< mbMaxDistance,
                              name = (\_ -> Just "Max Estimated Distance") =<< mbMaxDistance,
                              value = (Just . show) =<< mbMaxDistance
                            }
                        ]
                    }
                ]
          else Nothing,
      start =
        Init.StartInfo
          { location =
              Init.Location
                { gps =
                    Init.Gps
                      { lat = fromLoc.lat,
                        lon = fromLoc.lon
                      },
                  address = mkAddress fromLoc.address
                },
            authorization = Nothing
          },
      end =
        mbToLoc >>= \toLoc ->
          Just
            Init.StopInfo
              { location =
                  Init.Location
                    { gps =
                        Init.Gps
                          { lat = toLoc.lat,
                            lon = toLoc.lon
                          },
                      address = mkAddress toLoc.address
                    }
              },
      vehicle =
        Init.Vehicle
          { category = vehicleVariant
          }
    }

mkFulfillmentInfoV2 :: Init.FulfillmentType -> Maybe Text -> DL.Location -> Maybe DL.Location -> Maybe HighPrecMeters -> Init.VehicleVariant -> Init.FulfillmentInfoV2
mkFulfillmentInfoV2 fulfillmentType mbBppFullfillmentId fromLoc mbToLoc mbMaxDistance vehicleVariant =
  let start =
        Init.Stop
          { location =
              Init.Location
                { gps =
                    Init.Gps
                      { lat = fromLoc.lat,
                        lon = fromLoc.lon
                      },
                  address = mkAddress fromLoc.address
                },
            stopType = Init.START,
            authorization = Nothing
          }
      end =
        mbToLoc >>= \toLoc ->
          Just
            Init.Stop
              { location =
                  Init.Location
                    { gps =
                        Init.Gps
                          { lat = toLoc.lat,
                            lon = toLoc.lon
                          },
                      address = mkAddress toLoc.address
                    },
                stopType = Init.END,
                authorization = Nothing
              }
      end' = maybeToList end
   in Init.FulfillmentInfoV2
        { id = mbBppFullfillmentId,
          _type = fulfillmentType,
          tags =
            if isJust mbMaxDistance
              then
                Just
                  [ Init.TagGroupV2
                      { display = True,
                        descriptor =
                          Init.DescriptorV2
                            { code = Just "estimations",
                              name = Just "Estimations",
                              short_desc = Nothing
                            },
                        list =
                          [ Init.TagV2
                              { display = (\_ -> Just True) =<< mbMaxDistance,
                                descriptor =
                                  Just
                                    Init.DescriptorV2
                                      { code = (\_ -> Just "max_estimated_distance") =<< mbMaxDistance,
                                        name = (\_ -> Just "Max Estimated Distance") =<< mbMaxDistance,
                                        short_desc = Nothing
                                      },
                                value = (Just . show) =<< mbMaxDistance
                              }
                          ]
                      }
                  ]
              else Nothing,
          stops = [start] <> end',
          vehicle =
            Init.Vehicle
              { category = vehicleVariant
              }
        }

mkAddress :: DLA.LocationAddress -> Init.Address
mkAddress DLA.LocationAddress {..} =
  Init.Address
    { area_code = areaCode,
      locality = area,
      ward = ward,
      door = door,
      ..
    }

mkPayment :: Maybe DMPM.PaymentMethodInfo -> Init.Payment
mkPayment (Just DMPM.PaymentMethodInfo {..}) =
  Init.Payment
    { _type = Common.castDPaymentType paymentType,
      params =
        Init.PaymentParams
          { collected_by = Init.BPP,
            instrument = Just $ Common.castDPaymentInstrument paymentInstrument,
            currency = "INR",
            amount = Nothing
          },
      uri = Nothing
    }
-- for backward compatibility
mkPayment Nothing =
  Init.Payment
    { _type = Init.ON_FULFILLMENT,
      params =
        Init.PaymentParams
          { collected_by = Init.BPP,
            instrument = Nothing,
            currency = "INR",
            amount = Nothing
          },
      uri = Nothing
    }

mkPaymentV2 :: Maybe DMPM.PaymentMethodInfo -> Init.PaymentV2
mkPaymentV2 (Just DMPM.PaymentMethodInfo {..}) =
  Init.PaymentV2
    { --id = Nothing,
      _type = Common.castDPaymentType paymentType,
      collectedBy = Init.BPP,
      params =
        Init.PaymentParamsV2
          { -- collected_by = Init.BPP,
            instrument = Just $ Common.castDPaymentInstrument paymentInstrument,
            currency = "INR",
            amount = Nothing
          },
      uri = Nothing,
      status = Nothing,
      buyerAppFindeFeeType = Nothing,
      buyerAppFinderFeeAmount = Nothing,
      settlementDetails = Nothing
    }
-- for backward compatibility
mkPaymentV2 Nothing =
  Init.PaymentV2
    { --id = Nothing,
      _type = Init.ON_FULFILLMENT,
      collectedBy = Init.BPP,
      params =
        Init.PaymentParamsV2
          { -- collected_by = Init.BPP,
            instrument = Nothing,
            currency = "INR",
            amount = Nothing
          },
      uri = Nothing,
      status = Nothing,
      buyerAppFindeFeeType = Nothing,
      buyerAppFinderFeeAmount = Nothing,
      settlementDetails = Nothing
    }
