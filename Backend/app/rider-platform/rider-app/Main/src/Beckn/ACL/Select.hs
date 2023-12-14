{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}

module Beckn.ACL.Select (buildSelectReq, buildSelectReqV2) where

import Beckn.ACL.Common (castVariant, mkLocation)
import qualified Beckn.OnDemand.Utils.Common as Common
import qualified Beckn.Types.Core.Taxi.Select as Select
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Context as ContextV2
import Control.Lens ((%~))
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Domain.Action.UI.Search.Common as DSearchCommon
import qualified Domain.Action.UI.Select as DSelect
import qualified Domain.Types.Location as Location
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Beckn.Gps as Gps
import Kernel.Types.Beckn.ReqTypes
import Kernel.Types.Common
import Kernel.Utils.Common
import Tools.Error

buildSelectReq ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl]) =>
  DSelect.DSelectRes ->
  m (BecknReq Select.SelectMessage)
buildSelectReq dSelectRes = do
  let messageId = dSelectRes.estimate.bppEstimateId.getId
  let transactionId = dSelectRes.searchRequest.id.getId
  bapUrl <- asks (.nwAddress) <&> #baseUrlPath %~ (<> "/" <> T.unpack dSelectRes.merchant.id.getId)
  -- TODO :: Add request city, after multiple city support on gateway.
  context <- buildTaxiContext Context.SELECT messageId (Just transactionId) dSelectRes.merchant.bapId bapUrl (Just dSelectRes.providerId) (Just dSelectRes.providerUrl) dSelectRes.merchant.defaultCity dSelectRes.merchant.country dSelectRes.autoAssignEnabled
  order <- buildOrder dSelectRes
  pure $ BecknReq context $ Select.SelectMessage order

buildSelectReqV2 ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl]) =>
  DSelect.DSelectRes ->
  m Spec.SelectReq
buildSelectReqV2 dSelectRes = do
  let messageId = dSelectRes.estimate.bppEstimateId.getId
  let transactionId = dSelectRes.searchRequest.id.getId
  bapUrl <- asks (.nwAddress) <&> #baseUrlPath %~ (<> "/" <> T.unpack dSelectRes.merchant.id.getId)
  -- TODO :: Add request city, after multiple city support on gateway.
  context <- ContextV2.buildContextV2 Context.SELECT Context.MOBILITY messageId (Just transactionId) dSelectRes.merchant.bapId bapUrl (Just dSelectRes.providerId) (Just dSelectRes.providerUrl) dSelectRes.merchant.defaultCity dSelectRes.merchant.country
  message <- buildSelectReqMessage dSelectRes
  pure $ Spec.SelectReq {selectReqContext = context, selectReqMessage = message}

buildOrder :: (Monad m, Log m, MonadThrow m) => DSelect.DSelectRes -> m Select.Order
buildOrder res = do
  let start = mkLocation $ DSearchCommon.makeSearchReqLoc' res.searchRequest.fromLocation
  toLocation <- res.searchRequest.toLocation & fromMaybeM (InternalError "To location address not found")
  let end = mkLocation $ DSearchCommon.makeSearchReqLoc' toLocation
  let variant = castVariant res.variant
  let item =
        Select.OrderItem
          { id = res.estimate.itemId,
            price =
              Select.Price
                { currency = "INR",
                  value = show res.estimate.estimatedFare.getMoney
                },
            tags = if isJust res.customerExtraFee then Just $ Select.TG [mkCustomerTipTags] else Nothing
          }
  return
    Select.Order
      { items = [item],
        fulfillment =
          Select.FulfillmentInfo
            { start =
                Select.StartInfo
                  { location = start
                  },
              end =
                Select.StopInfo
                  { location = end
                  },
              id = res.estimate.bppEstimateId.getId,
              vehicle = Select.Vehicle {category = variant},
              _type = Select.RIDE
            }
      }
  where
    mkCustomerTipTags =
      Select.TagGroup
        { display = False,
          code = "customer_tip_info",
          name = "Customer Tip Info",
          list =
            [ Select.Tag
                { display = (\_ -> Just False) =<< res.customerExtraFee,
                  code = (\_ -> Just "customer_tip") =<< res.customerExtraFee,
                  name = (\_ -> Just "Customer Tip") =<< res.customerExtraFee,
                  value = (\charges -> Just $ show charges.getMoney) =<< res.customerExtraFee
                }
            ]
        }

buildSelectReqMessage :: (MonadFlow m) => DSelect.DSelectRes -> m Spec.ConfirmReqMessage
buildSelectReqMessage res = do
  selectMessageOrder <- tfOrder res
  pure $ Spec.ConfirmReqMessage {confirmReqMessageOrder = selectMessageOrder}

tfOrder :: (MonadFlow m) => DSelect.DSelectRes -> m Spec.Order
tfOrder res = do
  let orderBilling = Nothing
      orderCancellation = Nothing
      orderCancellationTerms = Nothing
      orderId = Nothing
      orderPayments = Nothing
      orderProvider = Nothing
      orderQuote = Nothing
      orderStatus = Nothing
      startLoc = res.searchRequest.fromLocation
  endLoc <- res.searchRequest.toLocation & fromMaybeM (InternalError "To location address not found")
  orderFulfillment <- tfFulfillment res startLoc endLoc
  orderItem <- tfOrderItem res
  pure $
    Spec.Order
      { orderFulfillments = Just [orderFulfillment],
        orderItems = Just [orderItem],
        ..
      }

tfFulfillment :: (MonadFlow m) => DSelect.DSelectRes -> Location.Location -> Location.Location -> m Spec.Fulfillment
tfFulfillment res startLoc endLoc = do
  let fulfillmentAgent = Nothing
      fulfillmentTags = Nothing
      fulfillmentState = Nothing
      fulfillmentCustomer = Nothing
      fulfillmentId = Just res.estimate.bppEstimateId.getId
      fulfillmentType = Just "RIDE"
      fulfillmentStops = mkStops startLoc endLoc
  fulfillmentVehicle <- tfVehicle res
  pure $
    Spec.Fulfillment
      { fulfillmentStops = fulfillmentStops,
        fulfillmentVehicle = Just fulfillmentVehicle,
        ..
      }

mkStops :: Location.Location -> Location.Location -> Maybe [Spec.Stop]
mkStops origin destination =
  let originGps = Gps.Gps {lat = origin.lat, lon = origin.lon}
      destinationGps d = Gps.Gps {lat = d.lat, lon = d.lon}
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
              stopAuthorization = Nothing
            },
          Spec.Stop
            { stopLocation =
                Just $
                  Spec.Location
                    { locationAddress = destination.address.building, -- JAYPAL, Confirm if it is correct to put it here
                      locationAreaCode = destination.address.areaCode,
                      locationCity = Just $ Spec.City Nothing destination.address.city,
                      locationCountry = Just $ Spec.Country Nothing destination.address.country,
                      locationGps = A.decode $ A.encode (destinationGps destination),
                      locationState = Just $ Spec.State destination.address.state,
                      locationId = Nothing -- JAYPAL, Not sure what to keep here
                    },
              stopType = Just "END",
              stopAuthorization = Nothing
            }
        ]

tfVehicle :: (MonadFlow m) => DSelect.DSelectRes -> m Spec.Vehicle
tfVehicle res = do
  let vehicleColor = Nothing
      vehicleMake = Nothing
      vehicleModel = Nothing
      vehicleRegistration = Nothing
      vehicleVariant = Nothing
      vehicleCategory = Common.castVehicleVariant res.variant
  pure $
    Spec.Vehicle
      { vehicleCategory = Just vehicleCategory,
        ..
      }

tfOrderItem :: (MonadFlow m) => DSelect.DSelectRes -> m Spec.Item
tfOrderItem res = do
  let itemDescriptor = Nothing
      itemFulfillmentIds = Nothing
      itemLocationIds = Nothing
      itemPaymentIds = Nothing
      itemId = Just res.estimate.itemId
      itemTags = if isJust res.customerExtraFee then Just [mkCustomerTipTagGroup res] else Nothing
  itemPrice <- tfPrice res
  pure $
    Spec.Item
      { itemPrice = Just itemPrice,
        ..
      }

mkCustomerTipTagGroup :: DSelect.DSelectRes -> Spec.TagGroup
mkCustomerTipTagGroup res =
  Spec.TagGroup
    { tagGroupDisplay = Just False,
      tagGroupDescriptor =
        Just $
          Spec.Descriptor
            { descriptorCode = Just "customer_tip_info",
              descriptorName = Just "Customer Tip Info",
              descriptorShortDesc = Nothing
            },
      tagGroupList =
        Just
          [ Spec.Tag
              { tagDescriptor =
                  Just $
                    Spec.Descriptor
                      { descriptorCode = (\_ -> Just "customer_tip") =<< res.customerExtraFee,
                        descriptorName = (\_ -> Just "Customer Tip") =<< res.customerExtraFee,
                        descriptorShortDesc = Nothing
                      },
                tagDisplay = Just False,
                tagValue = (\charges -> Just $ show charges.getMoney) =<< res.customerExtraFee
              }
          ]
    }

tfPrice :: (MonadFlow m) => DSelect.DSelectRes -> m Spec.Price
tfPrice res = do
  let priceCurrency = Just "INR"
      priceValue = Just $ show res.estimate.estimatedFare.getMoney
      priceComputedValue = Nothing
      priceMaximumValue = Nothing
      priceMinimumValue = Nothing
      priceOfferedValue = Nothing
  pure $
    Spec.Price {..}
