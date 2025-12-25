{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}

module Beckn.ACL.Update
  ( buildUpdateReq,
    UpdateBuildReq (..),
    UpdateBuildReqDetails (..),
    PaymentCompletedBuildReqDetails (..),
    EditLocationBuildReqDetails (..),
    AddStopBuildReqDetails (..),
    EditStopBuildReqDetails (..),
    UpdateStatus (..),
  )
where

import qualified Beckn.ACL.Common as Common
import qualified Beckn.OnDemand.Utils.Common as CommonUtils
import qualified BecknV2.OnDemand.Enums as Enums
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common as CommonUtils
import qualified BecknV2.OnDemand.Utils.Context as ContextV2
import Control.Lens ((%~))
import qualified Data.Text as T
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.Location as DLoc
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Ride as DRide
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
-- import Kernel.Types.Beckn.ReqTypes
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common

data UpdateBuildReq = UpdateBuildReq
  { bppBookingId :: Id DBooking.BPPBooking,
    bppId :: Text,
    bppUrl :: BaseUrl,
    transactionId :: Text,
    merchant :: DM.Merchant,
    messageId :: Text,
    city :: Context.City, -- Booking city, not merchant default city
    details :: UpdateBuildReqDetails
  }

data UpdateBuildReqDetails
  = UPaymentCompletedBuildReqDetails PaymentCompletedBuildReqDetails
  | UEditLocationBuildReqDetails EditLocationBuildReqDetails
  | UAddStopBuildReqDetails AddStopBuildReqDetails
  | UEditStopBuildReqDetails EditStopBuildReqDetails

data PaymentCompletedBuildReqDetails = PaymentCompletedBuildReqDetails
  { bppRideId :: Id DRide.BPPRide,
    paymentMethodInfo :: DMPM.PaymentMethodInfo
  }

data EditLocationBuildReqDetails = EditLocationBuildReqDetails
  { bppRideId :: Id DRide.BPPRide,
    origin :: Maybe DLoc.Location,
    destination :: Maybe DLoc.Location,
    stops :: Maybe [DLoc.Location],
    status :: UpdateStatus
  }

data UpdateStatus = SOFT_UPDATE | CONFIRM_UPDATE
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

newtype AddStopBuildReqDetails = AddStopBuildReqDetails
  { stops :: [DLoc.Location]
  }

newtype EditStopBuildReqDetails = EditStopBuildReqDetails
  { stops :: [DLoc.Location]
  }

buildUpdateReq ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl]) =>
  UpdateBuildReq ->
  m Spec.UpdateReq
buildUpdateReq res = do
  bapUrl <- asks (.nwAddress) <&> #baseUrlPath %~ (<> "/" <> T.unpack res.merchant.id.getId)
  ---------------Need to fix as per use case---------
  let ttl = CommonUtils.computeTtlISO8601 30
  context <-
    ContextV2.buildContextV2
      Context.UPDATE
      Context.MOBILITY
      res.messageId
      (Just res.transactionId)
      res.merchant.bapId
      bapUrl
      (Just res.bppId)
      (Just res.bppUrl)
      res.city
      res.merchant.country
      (Just ttl)

  pure $ Spec.UpdateReq context $ mkUpdateMessage res res.details

mkUpdateMessage ::
  UpdateBuildReq ->
  UpdateBuildReqDetails ->
  Spec.UpdateReqMessage
mkUpdateMessage req (UPaymentCompletedBuildReqDetails details) = do
  Spec.UpdateReqMessage
    { updateReqMessageOrder =
        Spec.Order
          { orderBilling = Nothing,
            orderCancellation = Nothing,
            orderCancellationTerms = Nothing,
            orderId = Just $ req.bppBookingId.getId,
            orderItems = Nothing,
            orderProvider = Nothing,
            orderCreatedAt = Nothing,
            orderUpdatedAt = Nothing,
            orderQuote = Nothing,
            orderStatus = Nothing,
            orderPayments =
              Just
                [ Spec.Payment
                    { paymentCollectedBy = Just $ show $ Common.castDPaymentCollector details.paymentMethodInfo.collectedBy,
                      paymentType = Just $ show $ Common.castDPaymentType details.paymentMethodInfo.paymentType,
                      paymentStatus = Just "PAID",
                      paymentId = Nothing,
                      paymentParams = Nothing,
                      paymentTags = Nothing,
                      paymentTlMethod = Nothing
                    }
                ],
            orderFulfillments =
              Just
                [ Spec.Fulfillment
                    { fulfillmentId = Just details.bppRideId.getId,
                      fulfillmentAgent = Nothing,
                      fulfillmentCustomer = Nothing,
                      fulfillmentState =
                        Just $
                          Spec.FulfillmentState
                            { fulfillmentStateDescriptor =
                                Just $
                                  Spec.Descriptor
                                    { descriptorCode = Just $ show Enums.PAYMENT_COMPLETED,
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
mkUpdateMessage req (UEditLocationBuildReqDetails details) = do
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
            orderCreatedAt = Nothing,
            orderUpdatedAt = Nothing,
            orderStatus = Just $ show details.status,
            orderPayments = Nothing,
            orderFulfillments =
              Just
                [ Spec.Fulfillment
                    { fulfillmentId = Just details.bppRideId.getId,
                      fulfillmentStops = CommonUtils.mkStops' details.origin (fromMaybe [] details.stops) details.destination,
                      fulfillmentAgent = Nothing,
                      fulfillmentCustomer = Nothing,
                      fulfillmentState =
                        Just $
                          Spec.FulfillmentState
                            { fulfillmentStateDescriptor =
                                Just $
                                  Spec.Descriptor
                                    { descriptorCode = Just $ show Enums.EDIT_LOCATION,
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
      updateReqMessageUpdateTarget = "order.fullfillments"
    }
mkUpdateMessage req (UAddStopBuildReqDetails details) = do
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
            orderCreatedAt = Nothing,
            orderUpdatedAt = Nothing,
            orderStatus = Nothing,
            orderPayments = Nothing,
            orderFulfillments =
              Just
                [ Spec.Fulfillment
                    { fulfillmentId = Nothing,
                      fulfillmentStops = Just $ map CommonUtils.makeStop details.stops,
                      fulfillmentAgent = Nothing,
                      fulfillmentCustomer = Nothing,
                      fulfillmentState =
                        Just $
                          Spec.FulfillmentState
                            { fulfillmentStateDescriptor =
                                Just $
                                  Spec.Descriptor
                                    { descriptorCode = Just $ show Enums.ADD_STOP,
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
      updateReqMessageUpdateTarget = "order.fullfillments"
    }
mkUpdateMessage req (UEditStopBuildReqDetails details) = do
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
            orderCreatedAt = Nothing,
            orderUpdatedAt = Nothing,
            orderFulfillments =
              Just
                [ Spec.Fulfillment
                    { fulfillmentId = Nothing,
                      fulfillmentStops = Just $ map CommonUtils.makeStop details.stops,
                      fulfillmentAgent = Nothing,
                      fulfillmentCustomer = Nothing,
                      fulfillmentState =
                        Just $
                          Spec.FulfillmentState
                            { fulfillmentStateDescriptor =
                                Just $
                                  Spec.Descriptor
                                    { descriptorCode = Just $ show Enums.EDIT_STOP,
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
      updateReqMessageUpdateTarget = "order.fullfillments"
    }
