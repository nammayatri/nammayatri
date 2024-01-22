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

module Beckn.ACL.FRFS.Confirm (buildConfirmReq) where

import qualified Beckn.ACL.FRFS.Utils as Utils
import qualified BecknV2.FRFS.Enums as Spec
import qualified BecknV2.FRFS.Types as Spec
import Data.List (singleton)
import qualified Domain.Types.FRFSTicketBooking as DBooking
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common

buildConfirmReq ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl]) =>
  DBooking.FRFSTicketBooking ->
  Text ->
  Text ->
  m (Spec.ConfirmReq)
buildConfirmReq booking txnId bapId = do
  let transactionId = booking.searchId.getId
  messageId <- generateGUID

  merchantId <- booking.merchantId <&> (.getId) & fromMaybeM (InternalError "MerchantId not found")
  context <- Utils.buildContext Spec.CONFIRM merchantId bapId transactionId messageId Nothing

  pure $
    Spec.ConfirmReq
      { confirmReqContext = context,
        confirmReqMessage = tfConfirmMessage booking txnId
      }

tfConfirmMessage :: DBooking.FRFSTicketBooking -> Text -> Spec.ConfirmReqMessage
tfConfirmMessage booking txnId =
  Spec.ConfirmReqMessage
    { confirmReqMessageOrder = tfOrder booking txnId
    }

tfOrder :: DBooking.FRFSTicketBooking -> Text -> Spec.Order
tfOrder booking txnId =
  Spec.Order
    { orderBilling = tfBilling booking,
      orderCancellationTerms = Nothing,
      orderCreatedAt = Nothing,
      orderFulfillments = Nothing,
      orderId = Nothing,
      orderItems = tfItems booking,
      orderPayments = tfPayments booking txnId,
      orderProvider = tfProvider booking,
      orderQuote = Nothing,
      orderStatus = Nothing,
      orderTags = Nothing,
      orderUpdatedAt = Nothing
    }

tfBilling :: DBooking.FRFSTicketBooking -> Maybe Spec.Billing
tfBilling _quote =
  Just $
    Spec.Billing
      { billingEmail = Nothing,
        billingName = Just "N/A", -- TODO: fix billing details
        billingPhone = Nothing
      }

tfItems :: DBooking.FRFSTicketBooking -> Maybe [Spec.Item]
tfItems booking =
  Just $
    [ Spec.Item
        { itemCategoryIds = Nothing,
          itemDescriptor = Nothing,
          itemFulfillmentIds = Nothing,
          itemId = Just booking.bppItemId,
          itemPrice = Nothing,
          itemQuantity = tfQuantity booking,
          itemTime = Nothing
        }
    ]

tfQuantity :: DBooking.FRFSTicketBooking -> Maybe Spec.ItemQuantity
tfQuantity booking =
  Just $
    Spec.ItemQuantity
      { itemQuantityMaximum = Nothing,
        itemQuantityMinimum = Nothing,
        itemQuantitySelected =
          Just $
            Spec.ItemQuantitySelected
              { itemQuantitySelectedCount = Just booking.quantity
              }
      }

tfPayments :: DBooking.FRFSTicketBooking -> Text -> Maybe [Spec.Payment]
tfPayments booking txnId =
  Just $
    singleton $
      Utils.mkPayment Spec.NOT_PAID (Just $ encodeToText booking.price) (Just txnId)

tfProvider :: DBooking.FRFSTicketBooking -> Maybe Spec.Provider
tfProvider booking =
  Just $
    Spec.Provider
      { providerCategories = Nothing,
        providerDescriptor = Nothing,
        providerFulfillments = Nothing,
        providerId = Just booking.providerId,
        providerItems = Nothing,
        providerPayments = Nothing,
        providerTags = Nothing,
        providerTime = Nothing
      }
