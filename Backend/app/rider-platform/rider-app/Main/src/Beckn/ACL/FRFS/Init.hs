{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Beckn.ACL.FRFS.Init (buildInitReq) where

import qualified Beckn.ACL.FRFS.Utils as Utils
import qualified BecknV2.FRFS.Enums as Spec
import qualified BecknV2.FRFS.Types as Spec
import Data.List (singleton)
import qualified Domain.Types.FRFSTicketBooking as DTBooking
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common

buildInitReq ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl]) =>
  DTBooking.FRFSTicketBooking ->
  Text ->
  m (Spec.InitReq)
buildInitReq tBooking bapId = do
  let transactionId = tBooking.searchId.getId
  messageId <- generateGUID

  merchantId <- tBooking.merchantId <&> (.getId) & fromMaybeM (InternalError "MerchantId not found")
  context <- Utils.buildContext Spec.INIT merchantId bapId transactionId messageId Nothing

  pure $
    Spec.InitReq
      { initReqContext = context,
        initReqMessage = tfInitMessage tBooking
      }

tfInitMessage :: DTBooking.FRFSTicketBooking -> Spec.ConfirmReqMessage
tfInitMessage tBooking =
  Spec.ConfirmReqMessage
    { confirmReqMessageOrder = tfOrder tBooking
    }

tfOrder :: DTBooking.FRFSTicketBooking -> Spec.Order
tfOrder tBooking =
  Spec.Order
    { orderBilling = tfBilling tBooking,
      orderCancellationTerms = Nothing,
      orderCreatedAt = Nothing,
      orderFulfillments = Nothing,
      orderId = Nothing,
      orderItems = tfItems tBooking,
      orderPayments = tfPayments tBooking,
      orderProvider = tfProvider tBooking,
      orderQuote = Nothing,
      orderStatus = Nothing,
      orderTags = Nothing,
      orderUpdatedAt = Nothing
    }

tfBilling :: DTBooking.FRFSTicketBooking -> Maybe Spec.Billing
tfBilling _quote =
  Just $
    Spec.Billing
      { billingEmail = Nothing,
        billingName = Just "N/A", -- TODO: fix billing details
        billingPhone = Nothing
      }

tfItems :: DTBooking.FRFSTicketBooking -> Maybe [Spec.Item]
tfItems tBooking =
  Just $
    [ Spec.Item
        { itemCategoryIds = Nothing,
          itemDescriptor = Nothing,
          itemFulfillmentIds = Nothing,
          itemId = Just tBooking.bppItemId,
          itemPrice = Nothing,
          itemQuantity = tfQuantity tBooking,
          itemTime = Nothing
        }
    ]

tfQuantity :: DTBooking.FRFSTicketBooking -> Maybe Spec.ItemQuantity
tfQuantity tBooking =
  Just $
    Spec.ItemQuantity
      { itemQuantityMaximum = Nothing,
        itemQuantityMinimum = Nothing,
        itemQuantitySelected =
          Just $
            Spec.ItemQuantitySelected
              { itemQuantitySelectedCount = Just tBooking.quantity
              }
      }

tfPayments :: DTBooking.FRFSTicketBooking -> Maybe [Spec.Payment]
tfPayments booking =
  Just $
    singleton $
      Utils.mkPayment Spec.NOT_PAID (Just $ encodeToText booking.price) Nothing

tfProvider :: DTBooking.FRFSTicketBooking -> Maybe Spec.Provider
tfProvider tBooking =
  Just $
    Spec.Provider
      { providerCategories = Nothing,
        providerDescriptor = Nothing,
        providerFulfillments = Nothing,
        providerId = Just tBooking.providerId,
        providerItems = Nothing,
        providerPayments = Nothing,
        providerTags = Nothing,
        providerTime = Nothing
      }
