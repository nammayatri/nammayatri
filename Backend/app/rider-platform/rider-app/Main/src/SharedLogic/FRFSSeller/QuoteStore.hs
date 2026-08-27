module SharedLogic.FRFSSeller.QuoteStore
  ( SellerQuote (..),
    SellerQuoteContext (..),
    persistQuotes,
    findQuote,
    holdQuote,
  )
where

import qualified API.Types.UI.FRFSTicketService as FRFSTypes
import qualified BecknV2.FRFS.Enums as SpecEnums
import Data.Aeson (encode)
import Data.ByteString.Lazy (toStrict)
import qualified Domain.Types.FRFSQuote as DQuote
import qualified Domain.Types.FRFSQuoteCategory as DQuoteCategory
import qualified Domain.Types.FRFSQuoteCategoryType as DQuoteCategoryType
import qualified Domain.Types.FRFSSearch as DSearch
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.StationType as DStationType
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.FRFSSeller.Common as Common
import qualified Storage.Queries.FRFSQuote as QQuote
import qualified Storage.Queries.FRFSQuoteCategory as QQuoteCategory
import qualified Storage.Queries.FRFSSearch as QSearch

-- | The seller's view of a priced catalog item. Backed by a real FRFSQuote row rather than
-- a Redis blob, so select/init/confirm read the same rows the app does and a cache flush
-- cannot lose an in-flight order.
data SellerQuote = SellerQuote
  { itemId :: Text,
    journeyId :: Text,
    providerId :: Text,
    providerName :: Text,
    fromStopCode :: Text,
    toStopCode :: Text,
    fromStopName :: Text,
    toStopName :: Text,
    priceValue :: Text,
    currency :: Text,
    maxTicketsPerOrder :: Int,
    fareQuoteId :: Maybe Text
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

data SellerQuoteContext = SellerQuoteContext
  { operator :: Text,
    transactionId :: Text,
    merchantId :: Id DM.Merchant,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    integratedBPPConfig :: DIBC.IntegratedBPPConfig,
    selfSubscriberId :: Text,
    selfSubscriberUrl :: Text,
    quantity :: Int,
    validTill :: UTCTime
  }

sellerSearchId :: Text -> Text -> Id DSearch.FRFSSearch
sellerSearchId operator transactionId = Id (Common.sellerSearchId operator transactionId)

persistQuotes :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => SellerQuoteContext -> [SellerQuote] -> m ()
persistQuotes ctx quotes = do
  now <- getCurrentTime
  let searchId = sellerSearchId ctx.operator ctx.transactionId
  existing <- QSearch.findById searchId
  when (isNothing existing) $ QSearch.create (mkSearch ctx searchId now (listToMaybe quotes))
  forM_ quotes $ \quote -> do
    quoteId <- generateGUID
    QQuote.create (mkQuote ctx searchId quoteId quote now)
    QQuoteCategory.create (mkQuoteCategory ctx quoteId quote now)

findQuote :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Text -> Text -> Text -> m (Maybe SellerQuote)
findQuote operator transactionId itemId = do
  quotes <- QQuote.findAllBySearchId (sellerSearchId operator transactionId)
  case find (\quote -> quote.bppItemId == itemId) quotes of
    Nothing -> pure Nothing
    Just quote -> do
      categories <- QQuoteCategory.findAllByQuoteIds [quote.id]
      pure (toSellerQuote quote <$> listToMaybe categories)

-- | Extends the quote's life once a buyer has initialised an order against it. The Redis
-- store used a second, longer TTL for this; here it is the row's own validTill.
holdQuote :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Int -> Text -> Text -> SellerQuote -> m ()
holdQuote heldTtl operator transactionId quote = do
  now <- getCurrentTime
  quotes <- QQuote.findAllBySearchId (sellerSearchId operator transactionId)
  forM_ (find (\row -> row.bppItemId == quote.itemId) quotes) $ \row ->
    QQuote.updateValidTillById row.id (addUTCTime (fromIntegral heldTtl) now)

toSellerQuote :: DQuote.FRFSQuote -> DQuoteCategory.FRFSQuoteCategory -> SellerQuote
toSellerQuote quote category =
  SellerQuote
    { itemId = quote.bppItemId,
      journeyId = fromMaybe "" quote.multimodalSearchRequestId,
      providerId = quote.providerId,
      providerName = quote.providerName,
      fromStopCode = quote.fromStationCode,
      toStopCode = quote.toStationCode,
      fromStopName = fromMaybe "" quote.fromStationName,
      toStopName = fromMaybe "" quote.toStationName,
      priceValue = Common.formatPrice (realToFrac category.price.amount),
      currency = show category.price.currency,
      maxTicketsPerOrder = category.selectedQuantity,
      fareQuoteId = category.holdId
    }

mkSearch :: SellerQuoteContext -> Id DSearch.FRFSSearch -> UTCTime -> Maybe SellerQuote -> DSearch.FRFSSearch
mkSearch ctx searchId now mbQuote =
  DSearch.FRFSSearch
    { busLocationData = [],
      clientBundleVersion = Nothing,
      clientSdkVersion = Nothing,
      cloudType = Nothing,
      fromStationAddress = Nothing,
      fromStationCode = maybe "" (.fromStopCode) mbQuote,
      fromStationName = mbQuote <&> (.fromStopName),
      fromStationPoint = Nothing,
      hasApplicablePass = Nothing,
      id = searchId,
      integratedBppConfigId = ctx.integratedBPPConfig.id,
      isOnSearchReceived = Nothing,
      isSingleMode = Nothing,
      merchantId = ctx.merchantId,
      merchantOperatingCityId = ctx.merchantOperatingCityId,
      multimodalSearchRequestId = Nothing,
      onSearchFailed = Nothing,
      partnerOrgId = Nothing,
      partnerOrgTransactionId = Nothing,
      quantity = ctx.quantity,
      recentLocationId = Nothing,
      riderId = Common.sellerRiderId,
      routeCode = Nothing,
      searchAsParentStops = Nothing,
      toStationAddress = Nothing,
      toStationCode = maybe "" (.toStopCode) mbQuote,
      toStationName = mbQuote <&> (.toStopName),
      toStationPoint = Nothing,
      validTill = Just ctx.validTill,
      vehicleNumber = Nothing,
      vehicleType = SpecEnums.METRO,
      createdAt = now,
      updatedAt = now
    }

mkQuote :: SellerQuoteContext -> Id DSearch.FRFSSearch -> Id DQuote.FRFSQuote -> SellerQuote -> UTCTime -> DQuote.FRFSQuote
mkQuote ctx searchId quoteId quote now =
  DQuote.FRFSQuote
    { _type = maybe DQuote.SingleJourney (.quoteType) (Common.journeyTypeForItemId quote.itemId),
      bppDelayedInterest = Nothing,
      bppItemId = quote.itemId,
      bppSubscriberId = ctx.selfSubscriberId,
      bppSubscriberUrl = ctx.selfSubscriberUrl,
      busLocationData = [],
      discountedTickets = Nothing,
      eventDiscountAmount = Nothing,
      fareDetails = Nothing,
      fromStationAddress = Nothing,
      fromStationCode = quote.fromStopCode,
      fromStationName = Just quote.fromStopName,
      fromStationPoint = Nothing,
      id = quoteId,
      integratedBppConfigId = ctx.integratedBPPConfig.id,
      merchantId = ctx.merchantId,
      merchantOperatingCityId = ctx.merchantOperatingCityId,
      multimodalSearchRequestId = Just quote.journeyId,
      offerSegment = Nothing,
      oldCacheDump = Nothing,
      partnerOrgId = Nothing,
      partnerOrgTransactionId = Nothing,
      providerDescription = Nothing,
      providerId = quote.providerId,
      providerName = quote.providerName,
      riderId = Common.sellerRiderId,
      routeStationsJson = Nothing,
      searchId = searchId,
      stationsJson = mkStationsJson ctx quote,
      toStationAddress = Nothing,
      toStationCode = quote.toStopCode,
      toStationName = Just quote.toStopName,
      toStationPoint = Nothing,
      validTill = ctx.validTill,
      vehicleNumber = Nothing,
      vehicleType = SpecEnums.METRO,
      createdAt = now,
      updatedAt = now
    }

-- | The START/END pair as the app's own station shape, so anything reading stations off a
-- seller quote gets real values rather than an empty list.
mkStationsJson :: SellerQuoteContext -> SellerQuote -> Text
mkStationsJson ctx quote =
  decodeUtf8 . toStrict . encode $
    [ station quote.fromStopCode quote.fromStopName DStationType.START 0,
      station quote.toStopCode quote.toStopName DStationType.END 1
    ]
  where
    station code name stationType sequenceNum =
      FRFSTypes.FRFSStationAPI
        { address = Nothing,
          code,
          color = Nothing,
          distance = Nothing,
          integratedBppConfigId = ctx.integratedBPPConfig.id,
          lat = Nothing,
          lon = Nothing,
          name = Just name,
          parentStopCode = Nothing,
          routeCodes = Nothing,
          routeDetails = Nothing,
          sequenceNum = Just sequenceNum,
          stationType = Just stationType,
          timeTakenToTravelUpcomingStop = Nothing,
          towards = Nothing
        }

mkQuoteCategory :: SellerQuoteContext -> Id DQuote.FRFSQuote -> SellerQuote -> UTCTime -> DQuoteCategory.FRFSQuoteCategory
mkQuoteCategory ctx quoteId quote now =
  DQuoteCategory.FRFSQuoteCategory
    { bppItemId = quote.itemId,
      category = DQuoteCategoryType.ADULT,
      categoryMeta = Nothing,
      finalPrice = Just price,
      holdId = quote.fareQuoteId,
      id = Id quoteId.getId,
      merchantId = ctx.merchantId,
      merchantOperatingCityId = ctx.merchantOperatingCityId,
      offeredPrice = price,
      price = price,
      quoteId = quoteId,
      seatIds = Nothing,
      seatLabels = Nothing,
      selectedQuantity = quote.maxTicketsPerOrder,
      createdAt = now,
      updatedAt = now
    }
  where
    amount = fromMaybe 0 (readMaybe (toString quote.priceValue) :: Maybe Double)
    price = Price {amountInt = round amount, amount = realToFrac amount, currency = INR}
