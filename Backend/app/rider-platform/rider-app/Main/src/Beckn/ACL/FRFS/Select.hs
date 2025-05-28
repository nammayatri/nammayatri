{-# LANGUAGE OverloadedLabels #-}

module Beckn.ACL.FRFS.Select (buildSelectReq) where

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

buildSelectReq ::
  (MonadFlow m) =>
  DTBooking.FRFSTicketBooking -> -- Booking info
  BecknConfig -> -- BAP config
  Utils.BppData -> -- BPP info (id, uri)
  Context.City -> -- City code
  m Spec.SelectReq
buildSelectReq booking bapConfig bppData city = do
  now <- getCurrentTime
  let transactionId = booking.searchId.getId
  let messageId = booking.id.getId
      ttl = diffUTCTime booking.validTill now

  let mSettlementType = bapConfig.settlementType
  context <- Utils.buildContext Spec.SELECT bapConfig transactionId messageId (Just $ Utils.durationToText ttl) (Just bppData) city

  pure $
    Spec.SelectReq
      { selectReqContext = context,
        selectReqMessage = tfSelectMessage booking mSettlementType
      }

tfSelectMessage :: DTBooking.FRFSTicketBooking -> Maybe Text -> Spec.ConfirmReqMessage
tfSelectMessage booking mSettlementType =
  Spec.ConfirmReqMessage
    { confirmReqMessageOrder = tfOrder booking mSettlementType
    }

tfOrder :: DTBooking.FRFSTicketBooking -> Maybe Text -> Spec.Order
tfOrder booking mSettlementType =
  Spec.Order
    { orderBilling = Nothing, -- No billing info needed for select
      orderCancellation = Nothing,
      orderCancellationTerms = Nothing,
      orderCreatedAt = Nothing,
      orderFulfillments = Nothing,
      orderId = Nothing,
      orderItems = tfItems booking,
      orderPayments = tfPayments booking mSettlementType,
      orderProvider = tfProvider booking,
      orderQuote = Nothing,
      orderStatus = Nothing,
      orderTags = Nothing,
      orderUpdatedAt = Nothing
    }

tfItems :: DTBooking.FRFSTicketBooking -> Maybe [Spec.Item]
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

tfQuantity :: DTBooking.FRFSTicketBooking -> Maybe Spec.ItemQuantity
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

tfPayments :: DTBooking.FRFSTicketBooking -> Maybe Text -> Maybe [Spec.Payment]
tfPayments booking mSettlementType = do
  let mCurrency = Just booking.price.currency
  Just $
    singleton $
      Utils.mkPaymentForSelectReq Spec.NOT_PAID (Just $ encodeToText booking.price.amount) Nothing Nothing mSettlementType mCurrency (show <$> booking.bppDelayedInterest)

tfProvider :: DTBooking.FRFSTicketBooking -> Maybe Spec.Provider
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
