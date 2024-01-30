{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wwarn=incomplete-record-updates #-}
{-# OPTIONS_GHC -Wwarn=incomplete-uni-patterns #-}

module Beckn.ACL.Update
  ( buildUpdateReq,
    UpdateBuildReq (..),
  )
where

import qualified Beckn.OnDemand.Utils.Common as CommonUtils
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Context as ContextV2
import Control.Lens ((%~))
import Data.Maybe ()
import qualified Data.Text as T
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.Location as DLoc
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Ride as DRide
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common

data UpdateBuildReq
  = PaymentCompletedBuildReq
      { bppBookingId :: Id DBooking.BPPBooking,
        bppRideId :: Id DRide.BPPRide,
        paymentMethodInfo :: DMPM.PaymentMethodInfo,
        bppId :: Text,
        bppUrl :: BaseUrl,
        transactionId :: Text,
        merchant :: DM.Merchant,
        city :: Context.City -- Booking city, not merchant default city
      }
  | EditLocationBuildReq
      { bppBookingId :: Id DBooking.BPPBooking,
        bppRideId :: Id DRide.BPPRide,
        origin :: Maybe DLoc.Location,
        destination :: Maybe DLoc.Location,
        bppId :: Text,
        bppUrl :: BaseUrl,
        transactionId :: Text,
        merchant :: DM.Merchant
      }

buildUpdateReq ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl]) =>
  UpdateBuildReq ->
  m Spec.UpdateReq
buildUpdateReq res = do
  messageId <- generateGUID
  bapUrl <- asks (.nwAddress) <&> #baseUrlPath %~ (<> "/" <> T.unpack res.merchant.id.getId)
  -- TODO :: Add request city, after multiple city support on gateway.
  context <-
    ContextV2.buildContextV2
      Context.UPDATE
      Context.MOBILITY
      messageId
      (Just res.transactionId)
      res.merchant.bapId
      bapUrl
      (Just res.bppId)
      (Just res.bppUrl)
      res.merchant.defaultCity
      res.merchant.country

  pure $ Spec.UpdateReq context $ mkUpdateMessage res

mkUpdateMessage ::
  UpdateBuildReq ->
  Spec.UpdateReqMessage
mkUpdateMessage req@PaymentCompletedBuildReq {} = do
  Spec.UpdateReqMessage
    { updateReqMessageOrder =
        Spec.Order
          { orderBilling = Nothing,
            orderCancellation = Nothing,
            orderCancellationTerms = Nothing,
            orderId = Just $ req.bppBookingId.getId,
            orderItems = Nothing,
            orderProvider = Nothing,
            orderQuote = Nothing,
            orderStatus = Nothing,
            orderPayments =
              Just
                [ Spec.Payment
                    { paymentCollectedBy = Just $ show req.paymentMethodInfo.collectedBy,
                      paymentType = Just $ show req.paymentMethodInfo.paymentType,
                      paymentStatus = Just "PAID",
                      paymentId = Nothing,
                      paymentParams = Nothing,
                      paymentTags = Nothing
                    }
                ],
            orderFulfillments =
              Just
                [ Spec.Fulfillment
                    { fulfillmentId = Just $ show req.bppRideId.getId,
                      fulfillmentAgent = Nothing,
                      fulfillmentCustomer = Nothing,
                      fulfillmentState =
                        Just $
                          Spec.FulfillmentState
                            { fulfillmentStateDescriptor =
                                Just $
                                  Spec.Descriptor
                                    { descriptorCode = Just "PAYMENT_COMPLETED",
                                      descriptorName = Nothing,
                                      descriptorShortDesc = Nothing
                                    }
                            },
                      fulfillmentStops = Nothing,
                      fulfillmentTags = Nothing,
                      fulfillmentType = Nothing,
                      fulfillmentVehicle = Nothing
                    }
                ]
          },
      updateReqMessageUpdateTarget = "order.payments, order.fullfillments"
    }
mkUpdateMessage req@EditLocationBuildReq {..} = do
  Spec.UpdateReqMessage
    { updateReqMessageOrder =
        Spec.Order
          { orderBilling = Nothing,
            orderCancellation = Nothing,
            orderCancellationTerms = Nothing,
            orderId = Just $ req.bppBookingId.getId,
            orderItems = Nothing,
            orderProvider = Nothing,
            orderQuote = Nothing,
            orderStatus = Nothing,
            orderPayments = Nothing,
            orderFulfillments =
              Just
                [ Spec.Fulfillment
                    { fulfillmentId = Just $ req.bppRideId.getId,
                      fulfillmentStops = Just $ mapMaybe (uncurry CommonUtils.mkStop) [(origin, Just "START"), (destination, Just "END")],
                      fulfillmentAgent = Nothing,
                      fulfillmentCustomer = Nothing,
                      fulfillmentState =
                        Just $
                          Spec.FulfillmentState
                            { fulfillmentStateDescriptor =
                                Just $
                                  Spec.Descriptor
                                    { descriptorCode = Just "EDIT_LOCATION",
                                      descriptorName = Nothing,
                                      descriptorShortDesc = Nothing
                                    }
                            },
                      fulfillmentTags = Nothing,
                      fulfillmentType = Nothing,
                      fulfillmentVehicle = Nothing
                    }
                ]
          },
      updateReqMessageUpdateTarget = "order.fulfillments.stops"
    }
