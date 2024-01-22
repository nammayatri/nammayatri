{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}

module Beckn.ACL.FRFS.Utils where

import qualified BecknV2.FRFS.Enums as Spec
import qualified BecknV2.FRFS.Types as Spec
import qualified BecknV2.FRFS.Utils as Utils
import Control.Lens ((%~))
import Data.Aeson as A
import qualified Data.Text as T
import qualified Domain.Action.Beckn.FRFS.Common as Domain
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common
import Kernel.Utils.Servant.BaseUrl

buildContext ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl]) =>
  Spec.Action ->
  Text ->
  Text ->
  Text ->
  Text ->
  Maybe Text ->
  m Spec.Context
buildContext action merchantId bapId txnId msgId mTTL = do
  now <- getCurrentTime
  bapUrl <- asks (.nwAddress) <&> #baseUrlPath %~ (<> "/" <> T.unpack merchantId) <&> showBaseUrlText
  return $
    Spec.Context
      { contextAction = encodeToText' action,
        contextBapId = Just bapId,
        contextBapUri = Just bapUrl,
        contextBppId = Nothing,
        contextBppUri = Nothing,
        contextDomain = encodeToText' Spec.FRFS,
        contextKey = Nothing,
        contextLocation = Nothing,
        contextMessageId = Just msgId,
        contextTimestamp = Just now,
        contextTransactionId = Just txnId,
        contextTtl = mTTL,
        contextVersion = Just "2.0.0"
      }

getStartStop :: [Spec.Stop] -> Maybe Spec.Stop
getStartStop stops = stops & find (\stop -> stop.stopType == start)
  where
    start = encodeToText' Spec.START

mkFareBreakup :: (MonadFlow m) => Spec.QuotationBreakupInner -> m Domain.DFareBreakUp
mkFareBreakup fareBreakup = do
  title <- fareBreakup.quotationBreakupInnerTitle & fromMaybeM (InvalidRequest "Title not found")
  price <- fareBreakup.quotationBreakupInnerPrice >>= Utils.parseMoney & fromMaybeM (InvalidRequest "Price not found")

  breakupItem <- fareBreakup.quotationBreakupInnerItem & fromMaybeM (InvalidRequest "BreakupItem not found")
  let pricePerUnit = breakupItem.itemPrice >>= Utils.parseMoney & fromMaybe price
  let quantity = breakupItem.itemQuantity >>= (.itemQuantitySelected) >>= (.itemQuantitySelectedCount) & fromMaybe 1

  pure $
    Domain.DFareBreakUp
      { title,
        price,
        pricePerUnit,
        quantity
      }

parseTickets :: (MonadFlow m) => Spec.Item -> [Spec.Fulfillment] -> m [Domain.DTicket]
parseTickets item fulfillments = do
  fulfillmentIds <- item.itemFulfillmentIds & fromMaybeM (InvalidRequest "FulfillmentIds not found")
  when (null fulfillmentIds) $ throwError $ InvalidRequest "Empty fulfillmentIds"

  let ticketFulfillments = filterByIds fulfillmentIds
  when (null ticketFulfillments) $ throwError $ InvalidRequest "No ticket fulfillment found"

  traverse parseTicket ticketFulfillments
  where
    filterByIds fIds = filter (\f -> f.fulfillmentId `elem` (Just <$> fIds)) fulfillments

parseTicket :: (MonadFlow m) => Spec.Fulfillment -> m Domain.DTicket
parseTicket fulfillment = do
  fId <- fulfillment.fulfillmentId & fromMaybeM (InvalidRequest "FulfillmentId not found")
  stops <- fulfillment.fulfillmentStops & fromMaybeM (InvalidRequest "FulfillmentStops not found")
  startStopAuth <- getStartStop stops >>= (.stopAuthorization) & fromMaybeM (InvalidRequest "StartStop Auth not found")

  qrData <- startStopAuth.authorizationToken & fromMaybeM (InvalidRequest "TicketQrData not found")
  validTill <- startStopAuth.authorizationValidTo & fromMaybeM (InvalidRequest "TicketValidTill not found")
  status <- startStopAuth.authorizationStatus & fromMaybeM (InvalidRequest "TicketStatus not found")

  tags <- fulfillment.fulfillmentTags & fromMaybeM (InvalidRequest "FulfillmentTags not found")
  ticketNumber <- Utils.getTag "TICKET_INFO" "NUMBER" tags & fromMaybeM (InvalidRequest "TicketNumber not found")

  pure $
    Domain.DTicket
      { qrData,
        validTill,
        bppFulfillmentId = fId,
        ticketNumber,
        status
      }

type TxnId = Text

type Amount = Text

mkPayment :: Spec.PaymentStatus -> Maybe Amount -> Maybe TxnId -> Spec.Payment
mkPayment paymentStatus mAmount mTxnId =
  Spec.Payment
    { paymentCollectedBy = Just "BAP",
      paymentId = Nothing,
      paymentParams = mTxnId >>= (\txnId -> mAmount <&> (mkPaymentParams txnId)),
      paymentStatus = encodeToText' paymentStatus,
      paymentTags = Just $ mkPaymentTags mAmount,
      paymentType = encodeToText' Spec.PRE_ORDER
    }

mkPaymentParams :: TxnId -> Amount -> Spec.PaymentParams
mkPaymentParams txnId amount =
  Spec.PaymentParams
    { paymentParamsAmount = Just amount,
      paymentParamsBankAccountNumber = Nothing,
      paymentParamsBankCode = Nothing,
      paymentParamsCurrency = Just "INR",
      paymentParamsTransactionId = Just txnId,
      paymentParamsVirtualPaymentAddress = Nothing
    }

mkPaymentTags :: Maybe Amount -> [Spec.TagGroup]
mkPaymentTags mAmount =
  [ mkBuyerFinderFeeTagGroup,
    mkSettlementTagGroup mAmount
  ]

mkBuyerFinderFeeTagGroup :: Spec.TagGroup
mkBuyerFinderFeeTagGroup =
  Spec.TagGroup
    { tagGroupDescriptor =
        Just $
          Spec.Descriptor
            { descriptorCode = Just "BUYER_FINDER_FEE",
              descriptorImages = Nothing,
              descriptorName = Nothing
            },
      tagGroupDisplay = Just False,
      tagGroupList = Just [feePercentage]
    }
  where
    feePercentage =
      Spec.Tag
        { tagDescriptor =
            Just $
              Spec.Descriptor
                { descriptorCode = Just "BUYER_FINDER_FEES_PERCENTAGE",
                  descriptorImages = Nothing,
                  descriptorName = Nothing
                },
          tagValue = Just "0"
        }

mkSettlementTagGroup :: Maybe Text -> Spec.TagGroup
mkSettlementTagGroup mAmount =
  Spec.TagGroup
    { tagGroupDescriptor =
        Just $
          Spec.Descriptor
            { descriptorCode = Just "SETTLEMENT_TERMS",
              descriptorImages = Nothing,
              descriptorName = Nothing
            },
      tagGroupDisplay = Just False,
      tagGroupList = Just settlementTags
    }
  where
    settlementTags =
      catMaybes
        [ mAmount <&> \amount ->
            Spec.Tag
              { tagDescriptor =
                  Just $
                    Spec.Descriptor
                      { descriptorCode = Just "SETTLEMENT_AMOUNT",
                        descriptorImages = Nothing,
                        descriptorName = Nothing
                      },
                tagValue = Just amount
              },
          Just $
            Spec.Tag
              { tagDescriptor =
                  Just $
                    Spec.Descriptor
                      { descriptorCode = Just "DELAY_INTEREST",
                        descriptorImages = Nothing,
                        descriptorName = Nothing
                      },
                tagValue = Just "0"
              },
          Just $
            Spec.Tag
              { tagDescriptor =
                  Just $
                    Spec.Descriptor
                      { descriptorCode = Just "STATIC_TERMS",
                        descriptorImages = Nothing,
                        descriptorName = Nothing
                      },
                tagValue = Just "https://api.example-bap.com/booking/terms" -- TODO: update with actual terms url
              }
        ]

encodeToText' :: (ToJSON a) => a -> Maybe Text
encodeToText' = A.decode . A.encode
