{-# LANGUAGE OverloadedLabels #-}

module Beckn.ACL.FRFS.Select (buildSelectReq) where

import qualified Beckn.ACL.FRFS.Utils as Utils
import qualified BecknV2.FRFS.Enums as Spec
import qualified BecknV2.FRFS.Types as Spec
import qualified BecknV2.FRFS.Utils as Utils
import Data.List (singleton)
import Domain.Types.BecknConfig
import qualified Domain.Types.FRFSQuote as DQuote
import Kernel.Prelude
import Kernel.Types.Beckn.Context as Context
import Kernel.Utils.Common

buildSelectReq ::
  (MonadFlow m) =>
  DQuote.FRFSQuote ->
  BecknConfig ->
  Utils.BppData ->
  Context.City ->
  m Spec.SelectReq
buildSelectReq quote bapConfig bppData city = do
  now <- getCurrentTime
  let transactionId = quote.searchId.getId
  let messageId = quote.id.getId
  let validTill = addUTCTime (intToNominalDiffTime (fromMaybe 120 bapConfig.selectTTLSec)) now
      ttl = diffUTCTime validTill now

  let mSettlementType = bapConfig.settlementType
  context <- Utils.buildContext Spec.SELECT bapConfig transactionId messageId (Just $ Utils.durationToText ttl) (Just bppData) city quote.vehicleType

  pure $
    Spec.SelectReq
      { selectReqContext = context,
        selectReqMessage = tfSelectMessage quote mSettlementType
      }

tfSelectMessage :: DQuote.FRFSQuote -> Maybe Text -> Spec.ConfirmReqMessage
tfSelectMessage quote mSettlementType =
  Spec.ConfirmReqMessage
    { confirmReqMessageOrder = tfOrder quote mSettlementType
    }

tfOrder :: DQuote.FRFSQuote -> Maybe Text -> Spec.Order
tfOrder quote mSettlementType =
  Spec.Order
    { orderBilling = Nothing,
      orderCancellation = Nothing,
      orderCancellationTerms = Nothing,
      orderCreatedAt = Nothing,
      orderFulfillments = Nothing,
      orderId = Nothing,
      orderItems = tfItems quote,
      orderPayments = tfPayments quote mSettlementType,
      orderProvider = tfProvider quote,
      orderQuote = Nothing,
      orderStatus = Nothing,
      orderTags = Nothing,
      orderUpdatedAt = Nothing
    }

tfItems :: DQuote.FRFSQuote -> Maybe [Spec.Item]
tfItems quote =
  Just $
    [ Spec.Item
        { itemCategoryIds = Nothing,
          itemDescriptor = Nothing,
          itemFulfillmentIds = Nothing,
          itemId = Just quote.bppItemId,
          itemPrice = Nothing,
          itemQuantity = tfQuantity quote,
          itemTime = Nothing
        }
    ]

tfQuantity :: DQuote.FRFSQuote -> Maybe Spec.ItemQuantity
tfQuantity quote =
  Just $
    Spec.ItemQuantity
      { itemQuantityMaximum = Nothing,
        itemQuantityMinimum = Nothing,
        itemQuantitySelected =
          Just $
            Spec.ItemQuantitySelected
              { itemQuantitySelectedCount = Just quote.quantity
              }
      }

tfPayments :: DQuote.FRFSQuote -> Maybe Text -> Maybe [Spec.Payment]
tfPayments quote mSettlementType = do
  let mCurrency = Just quote.price.currency
  Just $
    singleton $
      Utils.mkPaymentForSelectReq Spec.NOT_PAID (Just $ encodeToText quote.price.amount) Nothing Nothing mSettlementType mCurrency (show <$> quote.bppDelayedInterest)

tfProvider :: DQuote.FRFSQuote -> Maybe Spec.Provider
tfProvider quote =
  Just $
    Spec.Provider
      { providerCategories = Nothing,
        providerDescriptor = Nothing,
        providerFulfillments = Nothing,
        providerId = Just quote.providerId,
        providerItems = Nothing,
        providerPayments = Nothing,
        providerTags = Nothing,
        providerTime = Nothing
      }
