module Beckn.ACL.FRFSSeller.OnConfirm
  ( ConfirmedOrder (..),
    IssuedTicket (..),
    buildOnConfirmReq,
    buildOnConfirmErrorReq,
    mkOrder,
  )
where

import qualified Beckn.ACL.FRFS.Utils as ACLUtils
import qualified Beckn.ACL.FRFSSeller.OnInit as OnInit
import qualified Beckn.ACL.FRFSSeller.OnSearch as OnSearch
import qualified BecknV2.FRFS.Enums as Spec
import qualified BecknV2.FRFS.Types as Spec
import qualified Domain.Types as BknTypes
import Kernel.Prelude
import Kernel.Types.Common (Currency (..))
import Kernel.Types.TimeRFC339 (UTCTimeRFC3339 (..))

data IssuedTicket = IssuedTicket
  { fulfillmentId :: Text,
    ticketNumber :: Text,
    qrToken :: Text,
    qrStatus :: Text,
    validTill :: UTCTime
  }
  deriving (Show, Eq)

data ConfirmedOrder = ConfirmedOrder
  { billing :: Maybe Spec.Billing,
    courtJurisdiction :: Text,
    settlementWindow :: Maybe Text,
    buyerFinderFeePercentage :: Maybe Text,
    businessTermsUrl :: Text,
    cancellationTermsUrl :: Text,
    operatingWindow :: Maybe (UTCTime, UTCTime),
    fromStopLat :: Maybe Double,
    fromStopLon :: Maybe Double,
    toStopLat :: Maybe Double,
    toStopLon :: Maybe Double,
    maxPaidAreaMinutes :: Maybe Int,
    operatingHours :: [(Text, Text)],
    orderId :: Text,
    itemId :: Text,
    journeyTypeCode :: Text,
    journeyTypeName :: Text,
    providerId :: Text,
    providerName :: Text,
    unitPrice :: Text,
    currency :: Text,
    totalPrice :: Text,
    maxTicketsPerOrder :: Int,
    fromStopCode :: Text,
    fromStopName :: Text,
    toStopCode :: Text,
    toStopName :: Text,
    validityLabel :: Text,
    validityDuration :: Text,
    paymentId :: Text,
    paymentTxnId :: Text,
    account :: OnInit.SettlementAccount,
    authorizationType :: Text,
    createdAt :: UTCTime,
    tickets :: [IssuedTicket]
  }
  deriving (Show, Eq)

buildOnConfirmReq :: OnSearch.SellerIdentity -> UTCTime -> Spec.Context -> ConfirmedOrder -> Spec.OnConfirmReq
buildOnConfirmReq self now ctx order =
  Spec.OnConfirmReq
    { onConfirmReqContext = mkCallbackContext self now ctx,
      onConfirmReqError = Nothing,
      onConfirmReqMessage = Just (Spec.ConfirmReqMessage {confirmReqMessageOrder = mkOrder order})
    }

buildOnConfirmErrorReq :: OnSearch.SellerIdentity -> UTCTime -> Spec.Context -> Spec.Error -> Spec.OnConfirmReq
buildOnConfirmErrorReq self now ctx err =
  Spec.OnConfirmReq
    { onConfirmReqContext = mkCallbackContext self now ctx,
      onConfirmReqError = Just err,
      onConfirmReqMessage = Nothing
    }

mkCallbackContext :: OnSearch.SellerIdentity -> UTCTime -> Spec.Context -> Spec.Context
mkCallbackContext self now ctx =
  ctx{Spec.contextAction = ACLUtils.encodeToText' Spec.ON_CONFIRM,
      Spec.contextBppId = Just self.subscriberId,
      Spec.contextBppUri = Just self.subscriberUrl,
      Spec.contextTimestamp = Just (UTCTimeRFC3339 now),
      Spec.contextTtl = Just self.callbackTtl,
      Spec.contextVersion = Just self.contextVersion
     }

mkOrder :: ConfirmedOrder -> Spec.Order
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
      orderCreatedAt = Just order.createdAt,
      orderFulfillments = Just (map (mkFulfillment order) order.tickets),
      orderId = Just order.orderId,
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
      orderStatus = ACLUtils.encodeToText' Spec.ACTIVE,
      orderTags = Nothing,
      orderUpdatedAt = Just order.createdAt
    }

mkItem :: ConfirmedOrder -> Spec.Item
mkItem order =
  Spec.Item
    { itemCategoryIds = Just [OnInit.ticketCategoryId],
      itemDescriptor =
        Just
          Spec.Descriptor
            { descriptorCode = Just order.journeyTypeCode,
              descriptorImages = Nothing,
              descriptorName = Just order.journeyTypeName
            },
      itemFulfillmentIds = Just (map (.fulfillmentId) order.tickets),
      itemId = Just order.itemId,
      itemPrice = Just (OnInit.mkPrice order.currency order.unitPrice),
      itemQuantity =
        Just
          Spec.ItemQuantity
            { itemQuantityMaximum = Just Spec.ItemQuantityMaximum {itemQuantityMaximumCount = Just order.maxTicketsPerOrder},
              itemQuantityMinimum = Just Spec.ItemQuantityMinimum {itemQuantityMinimumCount = Just 1},
              itemQuantitySelected = Just Spec.ItemQuantitySelected {itemQuantitySelectedCount = Just (length order.tickets)}
            },
      itemTime =
        Just
          Spec.Time
            { timeDuration = Just order.validityDuration,
              timeLabel = Just order.validityLabel,
              timeRange = Nothing
            }
    }

mkFulfillment :: ConfirmedOrder -> IssuedTicket -> Spec.Fulfillment
mkFulfillment order ticket =
  Spec.Fulfillment
    { fulfillmentId = Just ticket.fulfillmentId,
      fulfillmentStops =
        Just
          [ OnInit.mkStop 1 Nothing Spec.START order.fromStopCode order.fromStopName order.fromStopLat order.fromStopLon (Just authorization),
            OnInit.mkStop 2 (Just "1") Spec.END order.toStopCode order.toStopName order.toStopLat order.toStopLon Nothing
          ],
      fulfillmentTags = Just [ticketInfoTag ticket],
      fulfillmentType = Just "TRIP",
      fulfillmentVehicle =
        Just
          Spec.Vehicle
            { vehicleCategory = ACLUtils.encodeToText' Spec.METRO,
              vehicleVariant = Nothing
            }
    }
  where
    authorization =
      Spec.Authorization
        { authorizationStatus = Just ticket.qrStatus,
          authorizationToken = Just ticket.qrToken,
          authorizationType = Just order.authorizationType,
          authorizationValidTo = Just ticket.validTill
        }

ticketInfoTag :: IssuedTicket -> Spec.TagGroup
ticketInfoTag ticket =
  Spec.TagGroup
    { tagGroupDescriptor = Just (mkDescriptor "TICKET_INFO"),
      tagGroupDisplay = Nothing,
      tagGroupList =
        Just
          [ Spec.Tag
              { tagDescriptor = Just (mkDescriptor "NUMBER"),
                tagValue = Just ticket.ticketNumber
              }
          ]
    }
  where
    mkDescriptor code =
      Spec.Descriptor
        { descriptorCode = Just code,
          descriptorImages = Nothing,
          descriptorName = Nothing
        }

mkQuote :: ConfirmedOrder -> Spec.Quotation
mkQuote order =
  Spec.Quotation
    { quotationPrice = Just (OnInit.mkPrice order.currency order.totalPrice),
      quotationBreakup =
        Just
          [ Spec.QuotationBreakupInner
              { quotationBreakupInnerTitle = Just "BASE_FARE",
                quotationBreakupInnerPrice = Just (OnInit.mkPrice order.currency order.totalPrice),
                quotationBreakupInnerItem = Just (mkItem order)
              }
          ]
    }

mkPayment :: ConfirmedOrder -> Spec.Payment
mkPayment order =
  ( ACLUtils.mkPaymentForConfirmReq
      Spec.PAID
      (Just order.account.settlementAmount)
      (Just order.paymentTxnId)
      (Just bknParams)
      order.account.settlementType
      (Just INR)
      Nothing
      (Just order.paymentId)
  )
    { Spec.paymentTags =
        Just
          ( OnSearch.sellerPaymentTags
              order.courtJurisdiction
              order.businessTermsUrl
              order.maxPaidAreaMinutes
              (Just order.account.settlementAmount)
              order.account.settlementType
              order.settlementWindow
              order.buyerFinderFeePercentage
          ),
      Spec.paymentType = Just "PRE-ORDER"
    }
  where
    bknParams =
      BknTypes.BknPaymentParams
        { bankAccNumber = Just order.account.bankAccountNumber,
          bankCode = Just order.account.bankCode,
          vpa = Nothing
        }
