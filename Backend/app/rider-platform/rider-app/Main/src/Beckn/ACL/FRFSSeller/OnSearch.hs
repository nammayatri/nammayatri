module Beckn.ACL.FRFSSeller.OnSearch
  ( SellerCatalog (..),
    SellerStop (..),
    SellerFulfillment (..),
    SellerItem (..),
    SellerIdentity (..),
    buildOnSearchReq,
    buildOnSearchErrorReq,
    sellerCallbackTtl,
    sellerContextVersion,
    mkProviderIdentity,
    sellerPaymentTags,
    mkGps,
    mkProviderTime,
    operatingHoursTags,
  )
where

import qualified Beckn.ACL.FRFS.Utils as ACLUtils
import qualified BecknV2.FRFS.Enums as Spec
import qualified BecknV2.FRFS.Types as Spec
import qualified BecknV2.FRFS.Utils as Utils
import qualified Data.Text as T
import Kernel.Prelude
import Kernel.Types.TimeRFC339 (UTCTimeRFC3339 (..))

data SellerItem = SellerItem
  { itemId :: Text,
    journeyTypeCode :: Text,
    itemDescription :: Text,
    priceValue :: Text,
    currency :: Text,
    maxTicketsPerOrder :: Int,
    fareQuoteId :: Maybe Text
  }
  deriving (Show, Eq)

data SellerStop = SellerStop
  { stopCode :: Text,
    stopCanonicalCode :: Text,
    stopName :: Text,
    stopLat :: Maybe Double,
    stopLon :: Maybe Double,
    stopType :: Spec.StopType
  }
  deriving (Show, Eq)

data SellerFulfillment = SellerFulfillment
  { fulfillmentId :: Text,
    stops :: [SellerStop]
  }
  deriving (Show, Eq)

data SellerCatalog = SellerCatalog
  { providerId :: Text,
    providerName :: Text,
    journeyId :: Maybe Text,
    validityLabel :: Text,
    validityDuration :: Text,
    brandName :: Text,
    brandLogoUrl :: Text,
    cancellationTermsUrl :: Text,
    courtJurisdiction :: Text,
    businessTermsUrl :: Text,
    maxPaidAreaMinutes :: Maybe Int,
    operatingHours :: [(Text, Text)],
    operatingWindow :: Maybe (UTCTime, UTCTime),
    fulfillments :: [SellerFulfillment],
    items :: [SellerItem]
  }
  deriving (Show, Eq)

data SellerIdentity = SellerIdentity
  { subscriberId :: Text,
    subscriberUrl :: Text
  }
  deriving (Show, Eq)

mkCallbackContext :: SellerIdentity -> UTCTime -> Spec.Context -> Spec.Context
mkCallbackContext self now ctx =
  ctx{Spec.contextAction = ACLUtils.encodeToText' Spec.ON_SEARCH,
      Spec.contextBppId = Just self.subscriberId,
      Spec.contextBppUri = Just self.subscriberUrl,
      Spec.contextTimestamp = Just (UTCTimeRFC3339 now),
      Spec.contextTtl = Just sellerCallbackTtl,
      Spec.contextVersion = Just sellerContextVersion
     }

sellerCallbackTtl :: Text
sellerCallbackTtl = "PT300S"

sellerContextVersion :: Text
sellerContextVersion = "2.0.0"

buildOnSearchReq :: SellerIdentity -> UTCTime -> Spec.Context -> SellerCatalog -> Spec.OnSearchReq
buildOnSearchReq self now ctx catalog =
  Spec.OnSearchReq
    { onSearchReqContext = mkCallbackContext self now ctx,
      onSearchReqError = Nothing,
      onSearchReqMessage = Just (mkMessage catalog)
    }

buildOnSearchErrorReq :: SellerIdentity -> UTCTime -> Spec.Context -> Spec.Error -> Spec.OnSearchReq
buildOnSearchErrorReq self now ctx err =
  Spec.OnSearchReq
    { onSearchReqContext = mkCallbackContext self now ctx,
      onSearchReqError = Just err,
      onSearchReqMessage = Nothing
    }

mkMessage :: SellerCatalog -> Spec.OnSearchReqMessage
mkMessage catalog =
  Spec.OnSearchReqMessage
    { onSearchReqMessageCatalog =
        Spec.Catalog
          { catalogDescriptor =
              Just
                Spec.Descriptor
                  { descriptorCode = Nothing,
                    descriptorImages = Just [Spec.Image {imageUrl = Just catalog.brandLogoUrl}],
                    descriptorName = Just catalog.brandName
                  },
            catalogProviders = Just [mkProvider catalog],
            catalogTags = Nothing
          }
    }

mkProviderIdentity :: Text -> Spec.Descriptor
mkProviderIdentity providerName =
  Spec.Descriptor
    { descriptorCode = Nothing,
      descriptorImages = Nothing,
      descriptorName = Just providerName
    }

mkProvider :: SellerCatalog -> Spec.Provider
mkProvider catalog =
  Spec.Provider
    { providerCancellationTerms =
        Just
          [ Spec.CancellationTerm
              { cancellationTermExternalRef =
                  Just Spec.MediaFile {mediaFileMimetype = Just "text/html", mediaFileUrl = Just catalog.cancellationTermsUrl}
              }
          ],
      providerCategories =
        Just
          [ Spec.Category
              { categoryDescriptor = Just Spec.Descriptor {descriptorCode = Just "TICKET", descriptorImages = Nothing, descriptorName = Nothing},
                categoryId = Just "TICKET"
              }
          ],
      providerDescriptor = Just (mkProviderIdentity catalog.providerName),
      providerFulfillments = Just (map (mkFulfillment catalog) catalog.fulfillments),
      providerId = Just catalog.providerId,
      providerItems = Just (map (mkItem catalog) catalog.items),
      providerPayments = Just [mkSellerPayment catalog],
      providerTags = operatingHoursTags catalog.operatingHours,
      providerTime = mkProviderTime catalog.operatingWindow
    }

mkSellerPayment :: SellerCatalog -> Spec.Payment
mkSellerPayment catalog =
  Spec.Payment
    { paymentCollectedBy = Just "BAP",
      paymentId = Nothing,
      paymentParams = Nothing,
      paymentStatus = Nothing,
      paymentTags = Just (sellerPaymentTags catalog.courtJurisdiction catalog.businessTermsUrl catalog.maxPaidAreaMinutes Nothing Nothing),
      paymentType = Nothing
    }

mkTag :: Text -> Text -> Spec.Tag
mkTag code value =
  Spec.Tag
    { tagDescriptor = Just Spec.Descriptor {descriptorCode = Just code, descriptorImages = Nothing, descriptorName = Nothing},
      tagValue = Just value
    }

sellerPaymentTags :: Text -> Text -> Maybe Int -> Maybe Text -> Maybe Text -> [Spec.TagGroup]
sellerPaymentTags courtJurisdiction' businessTermsUrl' mbMaxPaidAreaMinutes mbSettlementAmount mbSettlementType =
  buyerFinderFees : settlementTerms : journeyTerms
  where
    group code display list' =
      Spec.TagGroup
        { tagGroupDescriptor = Just Spec.Descriptor {descriptorCode = Just code, descriptorImages = Nothing, descriptorName = Nothing},
          tagGroupDisplay = display,
          tagGroupList = Just list'
        }

    buyerFinderFees =
      group
        "BUYER_FINDER_FEES"
        Nothing
        [ mkTag "BUYER_FINDER_FEES_PERCENTAGE" "0",
          mkTag "BUYER_FINDER_FEES_TYPE" "percent-annualized"
        ]

    settlementTerms =
      group
        "SETTLEMENT_TERMS"
        Nothing
        ( [ mkTag "SETTLEMENT_WINDOW" "PT1D",
            mkTag "DELAY_INTEREST" "0",
            mkTag "SETTLEMENT_BASIS" "INVOICE_RECEIPT",
            mkTag "MANDATORY_ARBITRATION" "TRUE",
            mkTag "COURT_JURISDICTION" courtJurisdiction',
            mkTag "STATIC_TERMS" businessTermsUrl'
          ]
            <> maybe [] (\amount -> [mkTag "SETTLEMENT_AMOUNT" amount]) mbSettlementAmount
            <> maybe [] (\t -> [mkTag "SETTLEMENT_TYPE" t]) mbSettlementType
        )

    journeyTerms =
      case mbMaxPaidAreaMinutes of
        Nothing -> []
        Just minutes -> [group "JOURNEY_TERMS" (Just True) [mkTag "MAX_PAID_AREA_TIME_MINUTES" (show minutes)]]

operatingHoursTags :: [(Text, Text)] -> Maybe [Spec.TagGroup]
operatingHoursTags [] = Nothing
operatingHoursTags hours =
  Just
    [ Spec.TagGroup
        { tagGroupDescriptor =
            Just
              Spec.Descriptor
                { descriptorCode = Just "OPERATING_HOURS",
                  descriptorImages = Nothing,
                  descriptorName = Just "Metro operating hours"
                },
          tagGroupDisplay = Just True,
          tagGroupList = Just (map (uncurry mkTag) hours)
        }
    ]

mkProviderTime :: Maybe (UTCTime, UTCTime) -> Maybe Spec.Time
mkProviderTime mbWindow =
  mbWindow <&> \(start, end) ->
    Spec.Time
      { timeDuration = Nothing,
        timeLabel = Nothing,
        timeRange = Just Spec.TimeRange {timeRangeStart = Just start, timeRangeEnd = Just end}
      }

mkFulfillment :: SellerCatalog -> SellerFulfillment -> Spec.Fulfillment
mkFulfillment catalog ful =
  Spec.Fulfillment
    { fulfillmentId = Just ful.fulfillmentId,
      fulfillmentStops = Just (zipWith mkStop [1 :: Int ..] ful.stops),
      fulfillmentTags = Nothing,
      fulfillmentType = Just (if null catalog.items then "ROUTE" else "TRIP"),
      fulfillmentVehicle =
        Just
          Spec.Vehicle
            { vehicleCategory = ACLUtils.encodeToText' Spec.METRO,
              vehicleVariant = Nothing
            }
    }

mkStop :: Int -> SellerStop -> Spec.Stop
mkStop order stop =
  Spec.Stop
    { stopAuthorization = Nothing,
      stopId = Just (show order),
      stopInstructions = Nothing,
      stopLocation =
        Just
          Spec.Location
            { locationDescriptor = Utils.tfDescriptor (Just stop.stopCode) (Just stop.stopName),
              locationGps = mkGps stop.stopLat stop.stopLon,
              locationCity = Nothing,
              locationCountry = Nothing
            },
      stopType = ACLUtils.encodeToText' stop.stopType,
      stopParentStopId = if order > 1 then Just (show (order - 1)) else Nothing
    }

mkGps :: Maybe Double -> Maybe Double -> Maybe Text
mkGps mbLat mbLon = do
  lat <- mbLat
  lon <- mbLon
  pure $ sixDp lat <> ", " <> sixDp lon
  where
    sixDp v =
      let scaled = round (toRational (abs v) * 1000000) :: Integer
          (whole, frac) = scaled `divMod` 1000000
          sign = if v < 0 then "-" else ""
       in sign <> show whole <> "." <> T.justifyRight 6 '0' (show frac)

mkItem :: SellerCatalog -> SellerItem -> Spec.Item
mkItem catalog item =
  Spec.Item
    { itemCategoryIds = Just ["TICKET"],
      itemDescriptor = Utils.tfDescriptor (Just item.journeyTypeCode) (Just item.itemDescription),
      itemFulfillmentIds = fmap (: []) catalog.journeyId,
      itemId = Just item.itemId,
      itemPrice =
        Just
          Spec.Price
            { priceCurrency = Just item.currency,
              priceValue = Just item.priceValue,
              priceOfferedValue = Nothing
            },
      itemQuantity =
        Just
          Spec.ItemQuantity
            { itemQuantityMaximum = Just Spec.ItemQuantityMaximum {itemQuantityMaximumCount = Just item.maxTicketsPerOrder},
              itemQuantityMinimum = Just Spec.ItemQuantityMinimum {itemQuantityMinimumCount = Just 1},
              itemQuantitySelected = Nothing
            },
      itemTime =
        Just
          Spec.Time
            { timeDuration = Just catalog.validityDuration,
              timeLabel = Just catalog.validityLabel,
              timeRange = Nothing
            }
    }
