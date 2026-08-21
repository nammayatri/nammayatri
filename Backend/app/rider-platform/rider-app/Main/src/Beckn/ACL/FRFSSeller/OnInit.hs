module Beckn.ACL.FRFSSeller.OnInit
  ( InitialisedOrder (..),
    SettlementAccount (..),
    buildOnInitReq,
    buildOnInitErrorReq,
    mkStop,
    mkPrice,
    ticketCategoryId,
  )
where

import qualified Beckn.ACL.FRFS.Utils as ACLUtils
import qualified Beckn.ACL.FRFSSeller.OnSearch as OnSearch
import qualified BecknV2.FRFS.Enums as Spec
import qualified BecknV2.FRFS.Types as Spec
import qualified BecknV2.FRFS.Utils as Utils
import qualified Data.Text as T
import qualified Domain.Types as BknTypes
import Kernel.Prelude
import Kernel.Types.TimeRFC339 (UTCTimeRFC3339 (..))

data SettlementAccount = SettlementAccount
  { bankAccountNumber :: Text,
    bankCode :: Text,
    settlementAmount :: Text,
    settlementType :: Maybe Text
  }
  deriving (Show, Eq)

data InitialisedOrder = InitialisedOrder
  { itemId :: Text,
    journeyId :: Text,
    journeyTypeCode :: Text,
    journeyTypeName :: Text,
    providerId :: Text,
    providerName :: Text,
    unitPrice :: Text,
    currency :: Text,
    quantity :: Int,
    maxTicketsPerOrder :: Int,
    totalPrice :: Text,
    fromStopCode :: Text,
    fromStopName :: Text,
    toStopCode :: Text,
    toStopName :: Text,
    validityLabel :: Text,
    validityDuration :: Text,
    paymentId :: Text,
    billing :: Maybe Spec.Billing,
    courtJurisdiction :: Text,
    businessTermsUrl :: Text,
    cancellationTermsUrl :: Text,
    operatingWindow :: Maybe (UTCTime, UTCTime),
    fromStopLat :: Maybe Double,
    fromStopLon :: Maybe Double,
    toStopLat :: Maybe Double,
    toStopLon :: Maybe Double,
    maxPaidAreaMinutes :: Maybe Int,
    operatingHours :: [(Text, Text)],
    account :: SettlementAccount
  }
  deriving (Show, Eq)

buildOnInitReq :: OnSearch.SellerIdentity -> UTCTime -> Spec.Context -> InitialisedOrder -> Spec.OnInitReq
buildOnInitReq self now ctx order =
  Spec.OnInitReq
    { onInitReqContext = mkCallbackContext self now ctx,
      onInitReqError = Nothing,
      onInitReqMessage = Just (Spec.ConfirmReqMessage {confirmReqMessageOrder = mkOrder order})
    }

buildOnInitErrorReq :: OnSearch.SellerIdentity -> UTCTime -> Spec.Context -> Text -> Text -> Spec.OnInitReq
buildOnInitErrorReq self now ctx code message =
  Spec.OnInitReq
    { onInitReqContext = mkCallbackContext self now ctx,
      onInitReqError =
        Just
          Spec.Error
            { errorCode = Just code,
              errorMessage = Just message,
              errorPaths = Nothing
            },
      onInitReqMessage = Nothing
    }

mkCallbackContext :: OnSearch.SellerIdentity -> UTCTime -> Spec.Context -> Spec.Context
mkCallbackContext self now ctx =
  ctx{Spec.contextAction = ACLUtils.encodeToText' Spec.ON_INIT,
      Spec.contextBppId = Just self.subscriberId,
      Spec.contextBppUri = Just self.subscriberUrl,
      Spec.contextTimestamp = Just (UTCTimeRFC3339 now),
      Spec.contextTtl = Just OnSearch.sellerCallbackTtl,
      Spec.contextVersion = Just OnSearch.sellerContextVersion
     }

mkOrder :: InitialisedOrder -> Spec.Order
mkOrder order =
  Spec.Order
    { orderBilling = order.billing,
      orderCancellation = Nothing,
      orderCancellationTerms =
        Just
          [ Spec.CancellationTerm
              { cancellationTermExternalRef =
                  Just Spec.MediaFile {mediaFileMimetype = Just "text/html", mediaFileUrl = Just order.cancellationTermsUrl}
              }
          ],
      orderCreatedAt = Nothing,
      orderFulfillments = Just [mkFulfillment order],
      orderId = Nothing,
      orderItems = Just [mkItem order],
      orderPayments = Just [mkPayment order],
      orderProvider =
        Just
          Spec.Provider
            { providerCancellationTerms = Nothing,
              providerCategories = Nothing,
              providerDescriptor = Just (OnSearch.mkProviderIdentity order.providerName),
              providerFulfillments = Nothing,
              providerId = Just order.providerId,
              providerItems = Nothing,
              providerPayments = Nothing,
              providerTags = OnSearch.operatingHoursTags order.operatingHours,
              providerTime = OnSearch.mkProviderTime order.operatingWindow
            },
      orderQuote = Just (mkQuote order),
      orderStatus = Nothing,
      orderTags = Nothing,
      orderUpdatedAt = Nothing
    }

mkItem :: InitialisedOrder -> Spec.Item
mkItem order =
  Spec.Item
    { itemCategoryIds = Just [ticketCategoryId],
      itemDescriptor =
        Just
          Spec.Descriptor
            { descriptorCode = Just order.journeyTypeCode,
              descriptorImages = Nothing,
              descriptorName = Just order.journeyTypeName
            },
      itemFulfillmentIds = Just [order.journeyId],
      itemId = Just order.itemId,
      itemPrice = Just (mkPrice order.currency order.unitPrice),
      itemQuantity =
        Just
          Spec.ItemQuantity
            { itemQuantityMaximum = Just Spec.ItemQuantityMaximum {itemQuantityMaximumCount = Just order.maxTicketsPerOrder},
              itemQuantityMinimum = Just Spec.ItemQuantityMinimum {itemQuantityMinimumCount = Just 1},
              itemQuantitySelected = Just Spec.ItemQuantitySelected {itemQuantitySelectedCount = Just order.quantity}
            },
      itemTime =
        Just
          Spec.Time
            { timeDuration = Just order.validityDuration,
              timeLabel = Just order.validityLabel,
              timeRange = Nothing
            }
    }

ticketCategoryId :: Text
ticketCategoryId = "TICKET"

mkFulfillment :: InitialisedOrder -> Spec.Fulfillment
mkFulfillment order =
  Spec.Fulfillment
    { fulfillmentId = Just order.journeyId,
      fulfillmentStops =
        Just
          [ mkStop 1 Nothing Spec.START order.fromStopCode order.fromStopName order.fromStopLat order.fromStopLon Nothing,
            mkStop 2 (Just "1") Spec.END order.toStopCode order.toStopName order.toStopLat order.toStopLon Nothing
          ],
      fulfillmentTags = Nothing,
      fulfillmentType = Just "TRIP",
      fulfillmentVehicle =
        Just
          Spec.Vehicle
            { vehicleCategory = ACLUtils.encodeToText' Spec.METRO,
              vehicleVariant = Nothing
            }
    }

mkStop :: Int -> Maybe Text -> Spec.StopType -> Text -> Text -> Maybe Double -> Maybe Double -> Maybe Spec.Authorization -> Spec.Stop
mkStop position parentId stopType code name mbLat mbLon mbAuthorization =
  Spec.Stop
    { stopAuthorization = mbAuthorization,
      stopId = Just (show position),
      stopInstructions = Nothing,
      stopLocation =
        Just
          Spec.Location
            { locationDescriptor = Utils.tfDescriptor (Just (publishedStationCode code)) (Just name),
              locationGps = OnSearch.mkGps mbLat mbLon,
              locationCity = Nothing,
              locationCountry = Nothing
            },
      stopParentStopId = parentId,
      stopType = ACLUtils.encodeToText' stopType
    }

publishedStationCode :: Text -> Text
publishedStationCode code = case T.splitOn "|" code of
  [_, bare] -> bare
  _ -> code

mkQuote :: InitialisedOrder -> Spec.Quotation
mkQuote order =
  Spec.Quotation
    { quotationPrice = Just (mkPrice order.currency order.totalPrice),
      quotationBreakup =
        Just
          [ Spec.QuotationBreakupInner
              { quotationBreakupInnerTitle = Just "BASE_FARE",
                quotationBreakupInnerPrice = Just (mkPrice order.currency order.totalPrice),
                quotationBreakupInnerItem = Just (mkItem order)
              }
          ]
    }

mkPayment :: InitialisedOrder -> Spec.Payment
mkPayment order =
  ( ACLUtils.mkPaymentForSearchReq
      Nothing
      (Just order.account.settlementAmount)
      (Just order.paymentId)
      (Just bknParams)
      order.account.settlementType
      Nothing
      Nothing
  )
    { Spec.paymentTags =
        Just
          ( OnSearch.sellerPaymentTags
              order.courtJurisdiction
              order.businessTermsUrl
              order.maxPaidAreaMinutes
              (Just order.account.settlementAmount)
              order.account.settlementType
          ),
      Spec.paymentStatus = Just "NOT-PAID",
      Spec.paymentType = Just "PRE-ORDER"
    }
  where
    bknParams =
      BknTypes.BknPaymentParams
        { bankAccNumber = Just order.account.bankAccountNumber,
          bankCode = Just order.account.bankCode,
          vpa = Nothing
        }

mkPrice :: Text -> Text -> Spec.Price
mkPrice currency value =
  Spec.Price
    { priceCurrency = Just currency,
      priceValue = Just value,
      priceOfferedValue = Nothing
    }
