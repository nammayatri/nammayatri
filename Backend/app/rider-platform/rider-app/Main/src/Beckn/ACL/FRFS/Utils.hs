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
import Control.Applicative ((<|>))
import Data.Aeson as A
import qualified Data.UUID as UU
import Domain.Action.Beckn.FRFS.Common
import qualified Domain.Action.Beckn.FRFS.Common as Domain
import Domain.Types
import Domain.Types.BecknConfig
import Domain.Types.FRFSQuoteCategoryType
import qualified Domain.Types.FRFSTicketBooking as Booking
import qualified Domain.Types.FRFSTicketStatus as Ticket
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
      pricePerUnit = breakupItem >>= (.itemPrice) >>= Utils.parseMoney & fromMaybe price
      quantity = breakupItem >>= (.itemQuantity) >>= (.itemQuantitySelected) >>= (.itemQuantitySelectedCount) & fromMaybe 1

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

  let ticketFulfillments = filterByIds "TICKET"
      finalTicketFulfillments = if not (null ticketFulfillments) then ticketFulfillments else filterByIds "TRIP"
  when (null finalTicketFulfillments) $ throwError $ InvalidRequest "No ticket fulfillment found"
  fallbackTicketNumber <- getTicketNumber
  return $ mapMaybe (parseTicket fallbackTicketNumber) finalTicketFulfillments
  where
    filterByIds fullfillmentType = filter (\f -> f.fulfillmentType == Just fullfillmentType) fulfillments

getTicketNumber :: (MonadFlow m) => m (Maybe Text)
getTicketNumber = do
  id <- generateGUID
  pure $
    UU.fromText id <&> \uuid -> show (fromIntegral ((\(a, b, c, d) -> a + b + c + d) (UU.toWords uuid)) :: Integer)

parseTicket :: Maybe Text -> Spec.Fulfillment -> Maybe Domain.DTicket
parseTicket fallbackTicketNumber fulfillment = do
  fId <- fulfillment.fulfillmentId
  stops <- fulfillment.fulfillmentStops
  startStopAuth <- getStartStop stops >>= (.stopAuthorization)

  qrData <- startStopAuth.authorizationToken
  validTill <- startStopAuth.authorizationValidTo
  status <- startStopAuth.authorizationStatus

  let mbTags = fulfillment.fulfillmentTags
  ticketNumber <- (mbTags >>= Utils.getTag "TICKET_INFO" "NUMBER") <|> fallbackTicketNumber
  pure $
    Domain.DTicket
      { qrData,
        vehicleNumber = Nothing,
        validTill,
        bppFulfillmentId = Just fId,
        ticketNumber,
        status,
        description = Nothing,
        qrRefreshAt = Nothing,
        commencingHours = Nothing
      }

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
                tagValue = Just "PT1D"
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
              }
        ]

encodeToText' :: (ToJSON a) => a -> Maybe Text
encodeToText' = A.decode . A.encode

type TicketNumber = Text

data TicketStatus = TicketStatus
  { ticketNumber :: TicketNumber,
    status :: Ticket.FRFSTicketStatus,
    vehicleNumber :: Maybe Text
  }

getTicketStatus :: (MonadFlow m) => Booking.FRFSTicketBooking -> Bool -> DTicket -> m TicketStatus
getTicketStatus booking checkInprogress dTicket = do
  let validTill = dTicket.validTill
  now <- getCurrentTime
  ticketStatus <- castTicketStatus dTicket.status booking checkInprogress booking.vehicleType
  if now > validTill && (ticketStatus `notElem` [Ticket.CANCELLED, Ticket.COUNTER_CANCELLED, Ticket.USED])
    then return TicketStatus {ticketNumber = dTicket.ticketNumber, status = Ticket.EXPIRED, vehicleNumber = dTicket.vehicleNumber}
    else return TicketStatus {ticketNumber = dTicket.ticketNumber, status = ticketStatus, vehicleNumber = dTicket.vehicleNumber}

castTicketStatus :: MonadFlow m => Text -> Booking.FRFSTicketBooking -> Bool -> Spec.VehicleCategory -> m Ticket.FRFSTicketStatus
castTicketStatus "UNCLAIMED" _ False _ = return Ticket.ACTIVE -- False means solicited on_status or on_confirm call
castTicketStatus "UNCLAIMED" _ True Spec.BUS = return Ticket.USED -- In case of bus, ticket is scanned only once to validate the booking
castTicketStatus "UNCLAIMED" _ True _ = return Ticket.INPROGRESS -- True means unsolicited on_status received
castTicketStatus "CLAIMED" _ _ Spec.METRO = return Ticket.USED
castTicketStatus "CANCELLED" booking _ _
  | booking.customerCancelled = return Ticket.CANCELLED
  | otherwise = return Ticket.COUNTER_CANCELLED
castTicketStatus "EXPIRED" _ _ _ = return Ticket.EXPIRED
castTicketStatus _ _ _ _ = throwError $ InternalError "Invalid ticket status"

data BppData = BppData
  { bppId :: Text,
    bppUri :: Text
  }
  deriving (Show, Eq, Generic)

mkCheckInprogressKey :: Text -> Text
mkCheckInprogressKey transactionId = "FRFS:OnStatus:Solicited-" <> transactionId

type ItemWithPrice = (Spec.Item, Price)

mkDCategorySelect :: (MonadFlow m) => ItemWithPrice -> m Domain.DCategorySelect
mkDCategorySelect (orderItem, quoteOfferedPrice) = do
  bppItemId' <- orderItem.itemId & fromMaybeM (InvalidRequest "BppItemId not found")
  quantity' <- orderItem.itemQuantity >>= (.itemQuantitySelected) >>= (.itemQuantitySelectedCount) & fromMaybeM (InvalidRequest "Item Quantity not found")
  let category =
        case orderItem.itemDescriptor >>= (.descriptorCode) of
          Just "SJT" -> ADULT
          Just "SFSJT" -> FEMALE
          _ -> ADULT
      itemOfferedPrice = orderItem.itemPrice >>= Utils.parseOfferPrice
      offeredPrice = fromMaybe quoteOfferedPrice itemOfferedPrice
  return $ DCategorySelect {bppItemId = bppItemId', quantity = quantity', category = category, price = fromMaybe (Price (Money 0) (HighPrecMoney 0.0) INR) (Just offeredPrice)}

getBreakupPrice :: MonadFlow m => Spec.QuotationBreakupInner -> m Price
getBreakupPrice b = b.quotationBreakupInnerPrice >>= Utils.parsePrice & fromMaybeM (InvalidRequest "Invalid breakup price")

computeOrderAdjustments :: MonadFlow m => [Spec.QuotationBreakupInner] -> m (Price, Price)
computeOrderAdjustments breakup = do
  let isToll b = b.quotationBreakupInnerTitle == Just "TOLL"
      isOffer b = b.quotationBreakupInnerTitle == Just "OFFER"
  tollPrices <- traverse getBreakupPrice (filter isToll breakup)
  offerPrices <- traverse getBreakupPrice (filter isOffer breakup)
  toll <- sumPrices tollPrices
  offer <- sumPrices offerPrices
  pure (toll, offer)

baseFareForItem :: MonadFlow m => Text -> [Spec.QuotationBreakupInner] -> m Price
baseFareForItem itemId breakup = do
  let isBaseFare b =
        b.quotationBreakupInnerTitle == Just "BASE_FARE"
          && (b.quotationBreakupInnerItem >>= (.itemId)) == Just itemId

  prices <- traverse getBreakupPrice (filter isBaseFare breakup)

  case prices of
    [] -> throwError (InvalidRequest ("BASE_FARE not found for item " <> itemId))
    _ -> sumPrices prices

zipItemsWithPrice :: MonadFlow m => [Spec.Item] -> [Spec.QuotationBreakupInner] -> m [(Spec.Item, Price)]
zipItemsWithPrice items breakup = do
  (toll, offer) <- computeOrderAdjustments breakup
  forM items $ \item -> do
    itemId <- item.itemId & fromMaybeM (InvalidRequest "ItemId not found")
    baseFare <- baseFareForItem itemId breakup
    finalWithToll <- addPrice baseFare toll
    finalPrice <- subtractPrice finalWithToll offer
    quantity <- item.itemQuantity >>= (.itemQuantitySelected) >>= (.itemQuantitySelectedCount) & fromMaybeM (InvalidRequest "Item Quantity not found")
    let totalPrice = modifyPrice finalPrice $ \p -> HighPrecMoney $ (p.getHighPrecMoney) * (toRational quantity)
    pure (item, fromMaybe totalPrice (item.itemPrice >>= Utils.parsePrice))

sumPrices :: (MonadThrow m, Log m) => [Price] -> m Price
sumPrices prices = withCurrencyCheckingList prices $ \mbCurrency amounts -> mkPrice mbCurrency (sum amounts)
