{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}

module Beckn.ACL.FRFS.Confirm (buildConfirmReq) where

import qualified Beckn.ACL.FRFS.Utils as Utils
import qualified BecknV2.FRFS.Enums as Spec
import qualified BecknV2.FRFS.Types as Spec
import qualified BecknV2.FRFS.Utils as Utils
import Data.List (singleton)
import Domain.Action.Beckn.FRFS.Common
import Domain.Types
import Domain.Types.BecknConfig
import qualified Domain.Types.FRFSTicketBooking as DBooking
import Kernel.Prelude
import Kernel.Types.Beckn.Context as Context
import Kernel.Utils.Common
import qualified SharedLogic.FRFSUtils as FRFSUtils

type RiderName = Text

type RiderNumber = Text

buildConfirmReq ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl]) =>
  (Maybe RiderName, Maybe RiderNumber) ->
  DBooking.FRFSTicketBooking ->
  BecknConfig ->
  Text ->
  Utils.BppData ->
  Context.City ->
  [DCategorySelect] ->
  m (Spec.ConfirmReq)
buildConfirmReq rider booking bapConfig txnId bppData city categories = do
  let transactionId = booking.searchId.getId
      messageId = booking.id.getId

  now <- getCurrentTime
  let confirmTtl = maybe booking.validTill (\ttlSec -> addUTCTime (intToNominalDiffTime ttlSec) now) bapConfig.confirmTTLSec
      ttl = diffUTCTime confirmTtl now
  let mPaymentParams = bapConfig.paymentParamsJson >>= decodeFromText
  let mSettlementType = bapConfig.settlementType
  context <- Utils.buildContext Spec.CONFIRM bapConfig transactionId messageId (Just $ Utils.durationToText ttl) (Just bppData) city booking.vehicleType

  pure $
    Spec.ConfirmReq
      { confirmReqContext = context,
        confirmReqMessage = tfConfirmMessage rider booking txnId mPaymentParams mSettlementType categories
      }

tfConfirmMessage :: (Maybe RiderName, Maybe RiderNumber) -> DBooking.FRFSTicketBooking -> Text -> Maybe BknPaymentParams -> Maybe Text -> [DCategorySelect] -> Spec.ConfirmReqMessage
tfConfirmMessage rider booking txnId mPaymentParams mSettlementType categories =
  Spec.ConfirmReqMessage
    { confirmReqMessageOrder = tfOrder rider booking txnId mPaymentParams mSettlementType categories
    }

tfOrder :: (Maybe RiderName, Maybe RiderNumber) -> DBooking.FRFSTicketBooking -> Text -> Maybe BknPaymentParams -> Maybe Text -> [DCategorySelect] -> Spec.Order
tfOrder rider booking txnId mPaymentParams mSettlementType categories =
  Spec.Order
    { orderBilling = tfBilling rider,
      orderCancellation = Nothing,
      orderCancellationTerms = Nothing,
      orderCreatedAt = Nothing,
      orderFulfillments = Nothing,
      orderId = Nothing,
      orderItems = tfItems booking categories,
      orderPayments = tfPayments booking categories txnId mPaymentParams mSettlementType,
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
      { billingEmail = Just "john.doe@example.com",
        billingName = Just (fromMaybe "NY User" mRiderName),
        billingPhone = mRiderNumber
      }

tfItems :: DBooking.FRFSTicketBooking -> [DCategorySelect] -> Maybe [Spec.Item]
tfItems _ categories =
  Just $
    map
      ( \category ->
          Spec.Item
            { itemCategoryIds = Nothing,
              itemDescriptor = Nothing,
              itemFulfillmentIds = Nothing,
              itemId = Just category.bppItemId,
              itemPrice = Nothing,
              itemQuantity = tfQuantity category.quantity,
              itemTime = Nothing
            }
      )
      categories

tfQuantity :: Int -> Maybe Spec.ItemQuantity
tfQuantity quantity =
  Just $
    Spec.ItemQuantity
      { itemQuantityMaximum = Nothing,
        itemQuantityMinimum = Nothing,
        itemQuantitySelected =
          Just $
            Spec.ItemQuantitySelected
              { itemQuantitySelectedCount = Just quantity
              }
      }

tfPayments :: DBooking.FRFSTicketBooking -> [DCategorySelect] -> Text -> Maybe BknPaymentParams -> Maybe Text -> Maybe [Spec.Payment]
tfPayments _ categories txnId mPaymentParams mSettlementType = do
  let fareParameters = FRFSUtils.mkFareParameters (FRFSUtils.mkCategoryPriceItemFromDCategorySelect categories)
  Just $
    singleton $
      Utils.mkPaymentForConfirmReq Spec.PAID (Just $ encodeToText fareParameters.totalPrice.amount) (Just txnId) mPaymentParams mSettlementType (Just fareParameters.currency) Nothing

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
