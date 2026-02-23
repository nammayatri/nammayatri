{-# LANGUAGE OverloadedLabels #-}

module Beckn.ACL.FRFS.Select (buildSelectReq) where

import qualified Beckn.ACL.FRFS.Utils as Utils
import qualified BecknV2.FRFS.Enums as Spec
import qualified BecknV2.FRFS.Types as Spec
import qualified BecknV2.FRFS.Utils as Utils
import Data.List (singleton)
import Domain.Action.Beckn.FRFS.Common
import Domain.Types.BecknConfig
import qualified Domain.Types.FRFSQuote as DQuote
import Kernel.Prelude
import Kernel.Types.Beckn.Context as Context
import Kernel.Utils.Common
import SharedLogic.FRFSFareCalculator

buildSelectReq ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl]) =>
  DQuote.FRFSQuote ->
  BecknConfig ->
  Utils.BppData ->
  Context.City ->
  [DCategorySelect] ->
  m Spec.SelectReq
buildSelectReq quote bapConfig bppData city categories = do
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
        selectReqMessage = tfSelectMessage quote mSettlementType categories
      }

tfSelectMessage :: DQuote.FRFSQuote -> Maybe Text -> [DCategorySelect] -> Spec.ConfirmReqMessage
tfSelectMessage quote mSettlementType categories =
  Spec.ConfirmReqMessage
    { confirmReqMessageOrder = tfOrder quote mSettlementType categories
    }

tfOrder :: DQuote.FRFSQuote -> Maybe Text -> [DCategorySelect] -> Spec.Order
tfOrder quote mSettlementType categories =
  Spec.Order
    { orderBilling = Nothing,
      orderCancellation = Nothing,
      orderCancellationTerms = Nothing,
      orderCreatedAt = Nothing,
      orderFulfillments = Nothing,
      orderId = Nothing,
      orderItems = tfItems quote categories,
      orderPayments = tfPayments quote categories mSettlementType,
      orderProvider = tfProvider quote,
      orderQuote = Nothing,
      orderStatus = Nothing,
      orderTags = Nothing,
      orderUpdatedAt = Nothing
    }

tfItems :: DQuote.FRFSQuote -> [DCategorySelect] -> Maybe [Spec.Item]
tfItems _ categories =
  Just $
    map
      ( \ondcReq ->
          Spec.Item
            { itemCategoryIds = Nothing,
              itemDescriptor = Nothing,
              itemFulfillmentIds = Nothing,
              itemId = Just ondcReq.bppItemId,
              itemPrice = Nothing,
              itemQuantity = tfQuantity ondcReq.quantity,
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

tfPayments :: DQuote.FRFSQuote -> [DCategorySelect] -> Maybe Text -> Maybe [Spec.Payment]
tfPayments quote categories mSettlementType = do
  let fareParameters = mkFareParameters (mkCategoryPriceItemFromDCategorySelect categories)
  Just $
    singleton $
      Utils.mkPaymentForSelectReq Spec.NOT_PAID (Just $ encodeToText fareParameters.totalPrice.amount) Nothing Nothing mSettlementType (Just fareParameters.currency) (show <$> quote.bppDelayedInterest)

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
