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
import qualified BecknV2.OnDemand.Enums as Enums
import Data.Aeson as A
import qualified Data.UUID as UU
import Domain.Action.Beckn.FRFS.Common
import qualified Domain.Action.Beckn.FRFS.Common as Domain
import Domain.Types
import Domain.Types.BecknConfig
import qualified Domain.Types.FRFSTicket as Ticket
import qualified Domain.Types.FRFSTicketBooking as Booking
import Kernel.Prelude
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.TimeRFC339
import Kernel.Utils.Common

buildContext ::
  (MonadFlow m) =>
  Spec.Action ->
  BecknConfig ->
  Text ->
  Text ->
  Maybe Text ->
  Maybe BppData ->
  Context.City ->
  Spec.VehicleCategory ->
  m Spec.Context
buildContext action bapConfig txnId msgId mTTL bppData city vehicleCategory = do
  now <- UTCTimeRFC3339 <$> getCurrentTime
  let bapUrl = showBaseUrl bapConfig.subscriberUrl
  let bapId = bapConfig.subscriberId
      contextBppId = bppData <&> (.bppId)
      contextBppUri = bppData <&> (.bppUri)
  cityCode <- getCodeFromCity city
  let contextVersion = if vehicleCategory == Spec.BUS then "2.0.1" else "2.0.0"
  return $
    Spec.Context
      { contextAction = encodeToText' action,
        contextBapId = Just bapId,
        contextBapUri = Just bapUrl,
        contextBppId,
        contextBppUri,
        contextDomain = encodeToText' Spec.FRFS,
        contextKey = Nothing,
        contextLocation = Just $ tfLocation cityCode,
        contextMessageId = Just msgId,
        contextTimestamp = Just now,
        contextTransactionId = Just txnId,
        contextTtl = mTTL,
        contextVersion = Just contextVersion
      }
  where
    getCodeFromCity city_ = do
      let cityCode = toJSON city_
      case cityCode of
        String code -> pure code
        _ -> throwError $ InvalidRequest "Incorrect city"

tfLocation :: Text -> Spec.Location
tfLocation location_code =
  Spec.Location
    { locationDescriptor = Nothing,
      locationGps = Nothing,
      locationCity =
        Just $
          Spec.City
            { cityCode = Just location_code,
              cityName = Nothing
            },
      locationCountry =
        Just $
          Spec.Country
            { countryCode = Just "IND",
              countryName = Nothing
            }
    }

getStartStop :: [Spec.Stop] -> Maybe Spec.Stop
getStartStop stops = stops & find (\stop -> stop.stopType == start)
  where
    start = encodeToText' Spec.START

mkFareBreakup :: (MonadFlow m) => Spec.QuotationBreakupInner -> m Domain.DFareBreakUp
mkFareBreakup fareBreakup = do
  title <- fareBreakup.quotationBreakupInnerTitle & fromMaybeM (InvalidRequest "Title not found")
  price <- fareBreakup.quotationBreakupInnerPrice >>= Utils.parseMoney & fromMaybeM (InvalidRequest "Price not found")

  let breakupItem = fareBreakup.quotationBreakupInnerItem
  let pricePerUnit = breakupItem >>= (.itemPrice) >>= Utils.parseMoney & fromMaybe price
  let quantity = breakupItem >>= (.itemQuantity) >>= (.itemQuantitySelected) >>= (.itemQuantitySelectedCount) & fromMaybe 1

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

  let ticketFulfillments = filterByIds fulfillmentIds "TICKET"
      finalTicketFulfillments = if not (null ticketFulfillments) then ticketFulfillments else filterByIds fulfillmentIds "TRIP"
  when (null finalTicketFulfillments) $ throwError $ InvalidRequest "No ticket fulfillment found"
  traverse parseTicket finalTicketFulfillments
  where
    filterByIds fIds fullfillmentType = filter (\f -> f.fulfillmentId `elem` (Just <$> fIds) && f.fulfillmentType == Just fullfillmentType) fulfillments

parseTicket :: (MonadFlow m) => Spec.Fulfillment -> m Domain.DTicket
parseTicket fulfillment = do
  fId <- fulfillment.fulfillmentId & fromMaybeM (InvalidRequest "FulfillmentId not found")
  stops <- fulfillment.fulfillmentStops & fromMaybeM (InvalidRequest "FulfillmentStops not found")
  startStopAuth <- getStartStop stops >>= (.stopAuthorization) & fromMaybeM (InvalidRequest "StartStop Auth not found")

  qrData <- startStopAuth.authorizationToken & fromMaybeM (InvalidRequest "TicketQrData not found")
  validTill <- startStopAuth.authorizationValidTo & fromMaybeM (InvalidRequest "TicketValidTill not found")
  status <- startStopAuth.authorizationStatus & fromMaybeM (InvalidRequest "TicketStatus not found")

  let mbTags = fulfillment.fulfillmentTags
  ticketNumber <- (pure (mbTags >>= Utils.getTag "TICKET_INFO" "NUMBER") |<|>| getTicketNumber) >>= fromMaybeM (InvalidRequest "TicketNumber not found")
  pure $
    Domain.DTicket
      { qrData,
        vehicleNumber = Nothing,
        validTill,
        bppFulfillmentId = fId,
        ticketNumber,
        status,
        description = Nothing,
        qrRefreshAt = Nothing
      }
  where
    getTicketNumber :: (MonadFlow m) => m (Maybe Text)
    getTicketNumber = do
      id <- generateGUID
      pure $
        UU.fromText id <&> \uuid -> show (fromIntegral ((\(a, b, c, d) -> a + b + c + d) (UU.toWords uuid)) :: Integer)

type TxnId = Text

type Amount = Text

type DelayInterest = Text

mkPayment :: Spec.PaymentStatus -> Maybe Amount -> Maybe TxnId -> Maybe BknPaymentParams -> Maybe Text -> Maybe Currency -> Maybe DelayInterest -> Spec.Payment
mkPayment paymentStatus mAmount mTxnId mPaymentParams mSettlementType mCurrency mbDelayInterest =
  Spec.Payment
    { paymentCollectedBy = Just $ show Enums.BAP,
      paymentId = mTxnId,
      paymentParams =
        if anyTrue [isJust mTxnId, isJust mAmount, isJust mPaymentParams]
          then Just $ mkPaymentParams mPaymentParams mTxnId mAmount mCurrency
          else Nothing,
      paymentStatus = encodeToText' paymentStatus,
      paymentTags = Just $ mkPaymentTags mSettlementType mAmount mbDelayInterest,
      paymentType = encodeToText' Spec.PRE_ORDER
    }
  where
    anyTrue = or

mkPaymentForInitReq :: Spec.PaymentStatus -> Maybe Amount -> Maybe TxnId -> Maybe BknPaymentParams -> Maybe Text -> Maybe Currency -> Maybe DelayInterest -> Spec.Payment
mkPaymentForInitReq paymentStatus mAmount _ _ mSettlementType _ mbDelayInterest =
  Spec.Payment
    { paymentCollectedBy = Just $ show Enums.BAP,
      paymentId = Nothing,
      paymentParams = Nothing,
      paymentStatus = encodeToText' paymentStatus,
      paymentTags = Just $ mkPaymentTags mSettlementType mAmount mbDelayInterest,
      paymentType = encodeToText' Spec.PRE_ORDER
    }

mkPaymentForSearchReq :: Maybe Spec.PaymentStatus -> Maybe Amount -> Maybe TxnId -> Maybe BknPaymentParams -> Maybe Text -> Maybe Currency -> Maybe DelayInterest -> Spec.Payment
mkPaymentForSearchReq _ mAmount mTxnId mPaymentParams mSettlementType mCurrency mbDelayInterest =
  Spec.Payment
    { paymentCollectedBy = Just $ show Enums.BAP,
      paymentId = mTxnId,
      paymentParams =
        if anyTrue [isJust mTxnId, isJust mAmount, isJust mPaymentParams]
          then Just $ mkPaymentParams mPaymentParams mTxnId mAmount mCurrency
          else Nothing,
      paymentStatus = Nothing,
      paymentTags = Just $ mkPaymentTags mSettlementType mAmount mbDelayInterest,
      paymentType = Nothing
    }
  where
    anyTrue = or

mkPaymentForSelectReq :: Spec.PaymentStatus -> Maybe Amount -> Maybe TxnId -> Maybe BknPaymentParams -> Maybe Text -> Maybe Currency -> Maybe DelayInterest -> Spec.Payment
mkPaymentForSelectReq paymentStatus _ _ _ _ _ _ =
  Spec.Payment
    { paymentCollectedBy = Just $ show Enums.BAP,
      paymentId = Nothing,
      paymentParams = Nothing,
      paymentStatus = encodeToText' paymentStatus,
      paymentTags = Nothing, --Just $ mkPaymentTags mSettlementType mAmount mbDelayInterest,
      paymentType = encodeToText' Spec.PRE_ORDER
    }

mkPaymentForConfirmReq :: Spec.PaymentStatus -> Maybe Amount -> Maybe TxnId -> Maybe BknPaymentParams -> Maybe Text -> Maybe Currency -> Maybe DelayInterest -> Spec.Payment
mkPaymentForConfirmReq paymentStatus mAmount mTxnId mPaymentParams mSettlementType mCurrency mbDelayInterest =
  Spec.Payment
    { paymentCollectedBy = Just $ show Enums.BAP,
      paymentId = mTxnId,
      paymentParams =
        if anyTrue [isJust mTxnId, isJust mAmount, isJust mPaymentParams]
          then Just $ mkPaymentParams mPaymentParams mTxnId mAmount mCurrency
          else Nothing,
      paymentStatus = encodeToText' paymentStatus,
      paymentTags = Just $ mkPaymentTags mSettlementType mAmount mbDelayInterest,
      paymentType = encodeToText' Spec.PRE_ORDER
    }
  where
    anyTrue = or

mkPaymentParams :: Maybe BknPaymentParams -> Maybe TxnId -> Maybe Amount -> Maybe Currency -> Spec.PaymentParams
mkPaymentParams mPaymentParams mTxnId mAmount mCurrency =
  Spec.PaymentParams
    { paymentParamsAmount = mAmount,
      paymentParamsBankAccountNumber = mPaymentParams >>= (.bankAccNumber),
      paymentParamsBankCode = mPaymentParams >>= (.bankCode),
      paymentParamsCurrency = show <$> mCurrency,
      paymentParamsTransactionId = mTxnId,
      paymentParamsVirtualPaymentAddress = mPaymentParams >>= (.vpa)
    }

mkPaymentTags :: Maybe Text -> Maybe Amount -> Maybe DelayInterest -> [Spec.TagGroup]
mkPaymentTags mSettlementType mAmount mbDelayInterest =
  catMaybes
    [ Just mkBuyerFinderFeeTagGroup,
      Just $ mkSettlementTagGroup mAmount mSettlementType mbDelayInterest
    ]

mkBuyerFinderFeeTagGroup :: Spec.TagGroup
mkBuyerFinderFeeTagGroup =
  Spec.TagGroup
    { tagGroupDescriptor =
        Just $
          Spec.Descriptor
            { descriptorCode = Just "BUYER_FINDER_FEES",
              descriptorImages = Nothing,
              descriptorName = Nothing
            },
      tagGroupDisplay = Just False,
      tagGroupList = Just [feePercentage, feeType]
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
    feeType =
      Spec.Tag
        { tagDescriptor =
            Just $
              Spec.Descriptor
                { descriptorCode = Just "BUYER_FINDER_FEES_TYPE",
                  descriptorImages = Nothing,
                  descriptorName = Nothing
                },
          tagValue = Just "percent"
        }

mkSettlementTagGroup :: Maybe Text -> Maybe Text -> Maybe Text -> Spec.TagGroup
mkSettlementTagGroup mAmount mSettlementType mbDelayInterest =
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
          mSettlementType <&> \settlementType ->
            Spec.Tag
              { tagDescriptor =
                  Just $
                    Spec.Descriptor
                      { descriptorCode = Just "SETTLEMENT_TYPE",
                        descriptorImages = Nothing,
                        descriptorName = Nothing
                      },
                tagValue = Just settlementType
              },
          Just $
            Spec.Tag
              { tagDescriptor =
                  Just $
                    Spec.Descriptor
                      { descriptorCode = Just "SETTLEMENT_WINDOW",
                        descriptorImages = Nothing,
                        descriptorName = Nothing
                      },
                tagValue = Just "PT24H"
              },
          mbDelayInterest <&> \delayInterest ->
            Spec.Tag
              { tagDescriptor =
                  Just $
                    Spec.Descriptor
                      { descriptorCode = Just "DELAY_INTEREST",
                        descriptorImages = Nothing,
                        descriptorName = Nothing
                      },
                tagValue = Just delayInterest
              },
          Just $
            Spec.Tag
              { tagDescriptor =
                  Just $
                    Spec.Descriptor
                      { descriptorCode = Just "SETTLEMENT_BASIS",
                        descriptorImages = Nothing,
                        descriptorName = Nothing
                      },
                tagValue = Just "DELIVERY"
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
              },
          Just $
            Spec.Tag
              { tagDescriptor =
                  Just $
                    Spec.Descriptor
                      { descriptorCode = Just "COURT_JURISDICTION",
                        descriptorImages = Nothing,
                        descriptorName = Nothing
                      },
                tagValue = Just "Cuttack"
              },
          Just $
            Spec.Tag
              { tagDescriptor =
                  Just $
                    Spec.Descriptor
                      { descriptorCode = Just "MANDATORY_ARBITRATION",
                        descriptorImages = Nothing,
                        descriptorName = Nothing
                      },
                tagValue = Just "true"
              }
        ]

encodeToText' :: (ToJSON a) => a -> Maybe Text
encodeToText' = A.decode . A.encode

type TicketNumber = Text

getTicketStatus :: (MonadFlow m) => Booking.FRFSTicketBooking -> DTicket -> m (TicketNumber, Ticket.FRFSTicketStatus, Maybe Text)
getTicketStatus booking dTicket = do
  let validTill = dTicket.validTill
  now <- getCurrentTime
  ticketStatus <- castTicketStatus dTicket.status booking
  if now > validTill && (ticketStatus /= Ticket.CANCELLED || ticketStatus /= Ticket.COUNTER_CANCELLED)
    then return (dTicket.ticketNumber, Ticket.EXPIRED, dTicket.vehicleNumber)
    else return (dTicket.ticketNumber, ticketStatus, dTicket.vehicleNumber)

castTicketStatus :: MonadFlow m => Text -> Booking.FRFSTicketBooking -> m Ticket.FRFSTicketStatus
castTicketStatus "UNCLAIMED" _ = return Ticket.ACTIVE
castTicketStatus "CLAIMED" _ = return Ticket.USED
castTicketStatus "CANCELLED" booking | booking.customerCancelled = return Ticket.CANCELLED
castTicketStatus "CANCELLED" booking | not booking.customerCancelled = return Ticket.COUNTER_CANCELLED
castTicketStatus _ _ = throwError $ InternalError "Invalid ticket status"

data BppData = BppData
  { bppId :: Text,
    bppUri :: Text
  }
  deriving (Show, Eq, Generic)
