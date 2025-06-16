{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}

module Beckn.ACL.FRFS.Init (buildInitReq) where

import qualified Beckn.ACL.FRFS.Utils as Utils
import qualified BecknV2.FRFS.Enums as Spec
import qualified BecknV2.FRFS.Types as Spec
import qualified BecknV2.FRFS.Utils as Utils
import Data.List (singleton)
import Domain.Types.BecknConfig
import qualified Domain.Types.FRFSTicketBooking as DTBooking
import Kernel.Prelude
import Kernel.Types.Beckn.Context as Context
import Kernel.Utils.Common

type RiderName = Text

type RiderNumber = Text

buildInitReq ::
  (MonadFlow m) =>
  (Maybe RiderName, Maybe RiderNumber) ->
  DTBooking.FRFSTicketBooking ->
  BecknConfig ->
  Utils.BppData ->
  Context.City ->
  m (Spec.InitReq)
buildInitReq rider tBooking bapConfig bppData city = do
  now <- getCurrentTime
  let transactionId = tBooking.searchId.getId
  let messageId = tBooking.id.getId
  let initTtl = maybe tBooking.validTill (\ttlSec -> addUTCTime (intToNominalDiffTime ttlSec) now) bapConfig.initTTLSec
      ttl = diffUTCTime initTtl now

  let mSettlementType = bapConfig.settlementType
  context <- Utils.buildContext Spec.INIT bapConfig transactionId messageId (Just $ Utils.durationToText ttl) (Just bppData) city tBooking.vehicleType

  pure $
    Spec.InitReq
      { initReqContext = context,
        initReqMessage = tfInitMessage rider tBooking mSettlementType
      }

tfInitMessage :: (Maybe RiderName, Maybe RiderNumber) -> DTBooking.FRFSTicketBooking -> Maybe Text -> Spec.ConfirmReqMessage
tfInitMessage rider tBooking mSettlementType =
  Spec.ConfirmReqMessage
    { confirmReqMessageOrder = tfOrder rider tBooking mSettlementType
    }

tfOrder :: (Maybe RiderName, Maybe RiderNumber) -> DTBooking.FRFSTicketBooking -> Maybe Text -> Spec.Order
tfOrder rider tBooking mSettlementType =
  Spec.Order
    { orderBilling = tfBilling rider,
      orderCancellation = Nothing,
      orderCancellationTerms = Nothing,
      orderCreatedAt = Nothing,
      orderFulfillments = Nothing,
      orderId = Nothing,
      orderItems = tfItems tBooking,
      orderPayments = tfPayments tBooking mSettlementType,
      orderProvider = tfProvider tBooking,
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

tfPayments :: DTBooking.FRFSTicketBooking -> Maybe Text -> Maybe [Spec.Payment]
tfPayments booking mSettlementType = do
  let mCurrency = Just booking.price.currency
  Just $
    singleton $
      Utils.mkPaymentForInitReq Spec.NOT_PAID (Just $ encodeToText booking.price.amount) Nothing Nothing mSettlementType mCurrency (show <$> booking.bppDelayedInterest)

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
