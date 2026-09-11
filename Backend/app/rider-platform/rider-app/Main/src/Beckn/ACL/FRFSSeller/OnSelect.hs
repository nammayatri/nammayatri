module Beckn.ACL.FRFSSeller.OnSelect
  ( SelectedItem (..),
    buildOnSelectReq,
    buildOnSelectErrorReq,
  )
where

import qualified Beckn.ACL.FRFS.Utils as ACLUtils
import Beckn.ACL.FRFSSeller.OnInit (mkPrice)
import qualified Beckn.ACL.FRFSSeller.OnInit as OnInit
import qualified Beckn.ACL.FRFSSeller.OnSearch as OnSearch
import qualified BecknV2.FRFS.Enums as Spec
import qualified BecknV2.FRFS.Types as Spec
import Kernel.Prelude
import Kernel.Types.TimeRFC339 (UTCTimeRFC3339 (..))

data SelectedItem = SelectedItem
  { itemId :: Text,
    journeyId :: Text,
    providerId :: Text,
    providerName :: Text,
    unitPrice :: Text,
    currency :: Text,
    quantity :: Int,
    totalPrice :: Text,
    settlementType :: Maybe Text,
    courtJurisdiction :: Text,
    settlementWindow :: Maybe Text,
    buyerFinderFeePercentage :: Maybe Text,
    businessTermsUrl :: Text,
    maxPaidAreaMinutes :: Maybe Int,
    operatingHours :: [(Text, Text)],
    operatingWindow :: Maybe (UTCTime, UTCTime),
    cancellationTermsUrl :: Text,
    journeyTypeCode :: Text,
    journeyTypeName :: Text,
    validityLabel :: Text,
    validityDuration :: Text,
    fromStopCode :: Text,
    fromStopName :: Text,
    fromStopLat :: Maybe Double,
    fromStopLon :: Maybe Double,
    toStopCode :: Text,
    toStopName :: Text,
    toStopLat :: Maybe Double,
    toStopLon :: Maybe Double
  }
  deriving (Show, Eq)

buildOnSelectReq :: OnSearch.SellerIdentity -> UTCTime -> Spec.Context -> SelectedItem -> Spec.OnSelectReq
buildOnSelectReq self now ctx selected =
  Spec.OnSelectReq
    { onSelectReqContext = mkCallbackContext self now ctx,
      onSelectReqError = Nothing,
      onSelectReqMessage = Just (Spec.ConfirmReqMessage {confirmReqMessageOrder = mkOrder selected})
    }

buildOnSelectErrorReq :: OnSearch.SellerIdentity -> UTCTime -> Spec.Context -> Spec.Error -> Spec.OnSelectReq
buildOnSelectErrorReq self now ctx err =
  Spec.OnSelectReq
    { onSelectReqContext = mkCallbackContext self now ctx,
      onSelectReqError = Just err,
      onSelectReqMessage = Nothing
    }

mkCallbackContext :: OnSearch.SellerIdentity -> UTCTime -> Spec.Context -> Spec.Context
mkCallbackContext self now ctx =
  ctx{Spec.contextAction = ACLUtils.encodeToText' Spec.ON_SELECT,
      Spec.contextBppId = Just self.subscriberId,
      Spec.contextBppUri = Just self.subscriberUrl,
      Spec.contextTimestamp = Just (UTCTimeRFC3339 now),
      Spec.contextTtl = Just self.callbackTtl,
      Spec.contextVersion = Just self.contextVersion
     }

mkOrder :: SelectedItem -> Spec.Order
mkOrder selected =
  Spec.Order
    { orderBilling = Nothing,
      orderCancellation = Nothing,
      orderCancellationTerms =
        Just
          [ Spec.CancellationTerm
              { cancellationTermExternalRef =
                  Just Spec.MediaFile {mediaFileMimetype = Just "text/html", mediaFileUrl = Just selected.cancellationTermsUrl}
              }
          ],
      orderCreatedAt = Nothing,
      orderFulfillments = Just [mkFulfillment selected],
      orderId = Nothing,
      orderItems = Just [mkItem selected],
      orderPayments = Nothing,
      orderProvider =
        Just
          Spec.Provider
            { providerCancellationTerms = Nothing,
              providerCategories = Nothing,
              providerDescriptor = Just (OnSearch.mkProviderIdentity selected.providerName),
              providerFulfillments = Nothing,
              providerId = Just selected.providerId,
              providerItems = Nothing,
              providerPayments =
                Just
                  [ Spec.Payment
                      { paymentCollectedBy = Just "BAP",
                        paymentId = Nothing,
                        paymentParams = Nothing,
                        paymentStatus = Nothing,
                        paymentTags =
                          Just
                            ( OnSearch.sellerPaymentTags
                                selected.courtJurisdiction
                                selected.businessTermsUrl
                                selected.maxPaidAreaMinutes
                                Nothing
                                Nothing
                                selected.settlementWindow
                                selected.buyerFinderFeePercentage
                            ),
                        paymentType = Nothing
                      }
                  ],
              providerTags = OnSearch.operatingHoursTags selected.operatingHours,
              providerTime = OnSearch.mkProviderTime selected.operatingWindow
            },
      orderQuote = Just (mkQuote selected),
      orderStatus = Nothing,
      orderTags = Nothing,
      orderUpdatedAt = Nothing
    }

mkItem :: SelectedItem -> Spec.Item
mkItem selected =
  Spec.Item
    { itemCategoryIds = Just ["TICKET"],
      itemDescriptor =
        Just
          Spec.Descriptor
            { descriptorCode = Just selected.journeyTypeCode,
              descriptorImages = Nothing,
              descriptorName = Just selected.journeyTypeName
            },
      itemFulfillmentIds = Just [selected.journeyId],
      itemId = Just selected.itemId,
      itemPrice = Just (mkPrice selected.currency selected.unitPrice),
      itemQuantity =
        Just
          Spec.ItemQuantity
            { itemQuantityMaximum = Nothing,
              itemQuantityMinimum = Nothing,
              itemQuantitySelected = Just Spec.ItemQuantitySelected {itemQuantitySelectedCount = Just selected.quantity}
            },
      itemTime =
        Just Spec.Time {timeDuration = Just selected.validityDuration, timeLabel = Just selected.validityLabel, timeRange = Nothing}
    }

mkFulfillment :: SelectedItem -> Spec.Fulfillment
mkFulfillment selected =
  Spec.Fulfillment
    { fulfillmentId = Just selected.journeyId,
      fulfillmentStops =
        Just
          [ OnInit.mkStop 1 Nothing Spec.START selected.fromStopCode selected.fromStopName selected.fromStopLat selected.fromStopLon Nothing,
            OnInit.mkStop 2 (Just "1") Spec.END selected.toStopCode selected.toStopName selected.toStopLat selected.toStopLon Nothing
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

mkQuote :: SelectedItem -> Spec.Quotation
mkQuote selected =
  Spec.Quotation
    { quotationPrice = Just (mkPrice selected.currency selected.totalPrice),
      quotationBreakup =
        Just
          [ Spec.QuotationBreakupInner
              { quotationBreakupInnerTitle = Just "BASE_FARE",
                quotationBreakupInnerPrice = Just (mkPrice selected.currency selected.totalPrice),
                quotationBreakupInnerItem = Just (mkItem selected)
              }
          ]
    }
