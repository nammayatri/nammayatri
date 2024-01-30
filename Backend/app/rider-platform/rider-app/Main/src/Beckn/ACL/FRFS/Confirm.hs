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
import qualified BecknV2.FRFS.Utils as Utils
import Data.List (singleton)
import Domain.Types.BecknConfig
import qualified Domain.Types.FRFSTicketBooking as DBooking
import Kernel.Prelude
import Kernel.Utils.Common

type RiderName = Text

type RiderNumber = Text

buildConfirmReq ::
  (MonadFlow m) =>
  (Maybe RiderName, Maybe RiderNumber) ->
  DBooking.FRFSTicketBooking ->
  BecknConfig ->
  Text ->
  Utils.BppData ->
  m (Spec.ConfirmReq)
buildConfirmReq rider booking bapConfig txnId bppData = do
  let transactionId = booking.searchId.getId
      messageId = booking.id.getId

  now <- getCurrentTime
  let ttl = diffUTCTime booking.validTill now
  let mPaymentParams = bapConfig.paymentParamsJson >>= decodeFromText
  let mSettlementType = bapConfig.settlementType
  context <- Utils.buildContext Spec.CONFIRM bapConfig transactionId messageId (Just $ Utils.durationToText ttl) (Just bppData)

  pure $
    Spec.ConfirmReq
      { confirmReqContext = context,
        confirmReqMessage = tfConfirmMessage rider booking txnId mPaymentParams mSettlementType
      }

tfConfirmMessage :: (Maybe RiderName, Maybe RiderNumber) -> DBooking.FRFSTicketBooking -> Text -> Maybe BknPaymentParams -> Maybe Text -> Spec.ConfirmReqMessage
tfConfirmMessage rider booking txnId mPaymentParams mSettlementType =
  Spec.ConfirmReqMessage
    { confirmReqMessageOrder = tfOrder rider booking txnId mPaymentParams mSettlementType
    }

tfOrder :: (Maybe RiderName, Maybe RiderNumber) -> DBooking.FRFSTicketBooking -> Text -> Maybe BknPaymentParams -> Maybe Text -> Spec.Order
tfOrder rider booking txnId mPaymentParams mSettlementType =
  Spec.Order
    { orderBilling = tfBilling rider,
      orderCancellationTerms = Nothing,
      orderCreatedAt = Nothing,
      orderFulfillments = Nothing,
      orderId = Nothing,
      orderItems = tfItems booking,
      orderPayments = tfPayments booking txnId mPaymentParams mSettlementType,
      orderProvider = tfProvider booking,
      orderQuote = Nothing,
      orderStatus = Nothing,
      orderTags = Nothing,
      orderUpdatedAt = Nothing
    }

tfBilling :: (Maybe RiderName, Maybe RiderNumber) -> Maybe Spec.Billing
tfBilling (mRiderName, mRiderNumber) =
  Just $
    Spec.Billing
      { billingEmail = Nothing,
        billingName = mRiderName,
        billingPhone = mRiderNumber
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

tfPayments :: DBooking.FRFSTicketBooking -> Text -> Maybe BknPaymentParams -> Maybe Text -> Maybe [Spec.Payment]
tfPayments booking txnId mPaymentParams mSettlementType =
  Just $
    singleton $
      Utils.mkPayment Spec.PAID (Just $ encodeToText booking.price) (Just txnId) mPaymentParams mSettlementType

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
