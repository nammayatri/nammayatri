module Domain.Action.Beckn.FRFSSeller.Search (handleSearch) where

import qualified Beckn.ACL.FRFS.Utils as FRFSUtilsACL
import qualified Beckn.ACL.FRFSSeller.OnSearch as ACL
import qualified BecknV2.FRFS.Enums as SpecEnums
import qualified BecknV2.FRFS.Types as Spec
import qualified BecknV2.OnDemand.Enums as BecknSpec
import Control.Applicative ((<|>))
import qualified Data.Aeson as A
import Data.List (groupBy, sortOn)
import qualified Data.Text as T
import qualified Domain.Action.Beckn.FRFSSeller.Init as Init
import Domain.Types.FRFSQuoteCategoryType (FRFSQuoteCategoryType (ADULT))
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Environment (Flow)
import qualified ExternalBPP.ExternalAPI.CallAPI as ExternalCallAPI
import qualified ExternalBPP.ExternalAPI.Metro.CMRL.StationList as CMRLStationList
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.FRFSSeller.CallBAP as CallBAP
import qualified SharedLogic.FRFSSeller.Common as Common
import qualified SharedLogic.FRFSSeller.QuoteStore as QuoteStore
import qualified SharedLogic.FRFSUtils as FRFSUtils
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified Storage.CachedQueries.BecknConfig as QBC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Tools.Error
import qualified Tools.Metrics.BAPMetrics as Metrics

handleSearch :: Text -> Spec.SearchReq -> Flow ()
handleSearch operator req = do
  let ctx = req.searchReqContext
  bapUriText <-
    ctx.contextBapUri
      & fromMaybeM (InvalidRequest "BapUri missing on search context")
  bapUri <- parseBaseUrl bapUriText
  merchantId <-
    (.id)
      <$> ( Common.findSellerMerchant operator
              >>= fromMaybeM (MerchantDoesNotExist operator)
          )
  becknConfig <-
    QBC.findByMerchantIdDomainAndVehicle merchantId (show SpecEnums.FRFS) BecknSpec.METRO
      >>= fromMaybeM (BecknConfigNotFound $ "merchantId:" <> merchantId.getId <> " domain:FRFS vehicle:METRO")
  mbOperatorConfig <- (.operatorConfig) <$> Init.sellerIntegratedBPPConfig merchantId ctx
  let self =
        ACL.SellerIdentity
          { subscriberId = becknConfig.subscriberId,
            subscriberUrl = showBaseUrl becknConfig.subscriberUrl,
            callbackTtl = Common.callbackTtl becknConfig.searchTTLSec,
            contextVersion = Common.contextVersionOf mbOperatorConfig
          }
  transactionId <-
    ctx.contextTransactionId
      & fromMaybeM (InvalidRequest "TransactionId missing on search context")
  let cityCode = fromMaybe "" (ctx.contextLocation >>= (.locationCity) >>= (.cityCode))
  now' <- getCurrentTime
  case Common.serviceabilityOf (mbOperatorConfig >>= (.serviceableBapIds)) cityCode ctx.contextBapId of
    Common.Unserviceable reason -> do
      logWarning $ "FRFS seller search refused: " <> reason
      CallBAP.sendOnSearch merchantId self.subscriberId bapUri $
        ACL.buildOnSearchErrorReq self now' ctx (Common.becknError Common.LocationUnserviceable reason)
    Common.Serviceable -> serve ctx self bapUri merchantId transactionId
  where
    serve ctx self bapUri merchantId transactionId = do
      catalogResult <- withTryCatch "frfsSeller:buildCatalog" (buildCatalog operator merchantId transactionId self req)
      now <- getCurrentTime
      onSearchReq <- case catalogResult of
        Right catalog -> pure $ ACL.buildOnSearchReq self now ctx catalog
        Left err -> do
          logError $ "frfsSeller:buildCatalog failed for operator " <> operator <> ": " <> show err
          pure $ ACL.buildOnSearchErrorReq self now ctx (Common.becknError Common.InternalError "Unable to build catalog for this search")
      CallBAP.sendOnSearch merchantId self.subscriberId bapUri onSearchReq

mkQuotes :: ACL.SellerCatalog -> [QuoteStore.SellerQuote]
mkQuotes catalog =
  [ QuoteStore.SellerQuote
      { itemId = item.itemId,
        journeyId = fromMaybe "" catalog.journeyId,
        providerId = catalog.providerId,
        providerName = catalog.providerName,
        fromStopCode = fromMaybe "" (stopOf SpecEnums.START <&> (.stopCanonicalCode)),
        toStopCode = fromMaybe "" (stopOf SpecEnums.END <&> (.stopCanonicalCode)),
        fromStopName = fromMaybe "" (stopOf SpecEnums.START <&> (.stopName)),
        toStopName = fromMaybe "" (stopOf SpecEnums.END <&> (.stopName)),
        priceValue = item.priceValue,
        currency = item.currency,
        fareQuoteId = item.fareQuoteId
      }
    | item <- catalog.items
  ]
  where
    stopOf wanted = find (\stop -> stop.stopType == wanted) (concatMap (.stops) catalog.fulfillments)

buildCatalog :: Text -> Id DM.Merchant -> Text -> ACL.SellerIdentity -> Spec.SearchReq -> Flow ACL.SellerCatalog
buildCatalog operator merchantId transactionId self req = do
  cityCode <-
    (req.searchReqContext.contextLocation >>= (.locationCity) >>= (.cityCode))
      & fromMaybeM (InvalidRequest "City missing on search context")
  city <- case A.fromJSON (A.String cityCode) of
    A.Success c -> pure c
    A.Error e -> throwError (InvalidRequest $ "Unparseable city code " <> cityCode <> ": " <> show e)
  merchantOperatingCity <-
    CQMOC.findByMerchantIdAndCity merchantId city
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant:" <> merchantId.getId <> " city:" <> show city)
  mbJourney <- traverse originAndDestination (intentStops req)
  integratedBPPConfig <-
    SIBC.findIntegratedBPPConfig Nothing merchantOperatingCity.id BecknSpec.METRO DIBC.MULTIMODAL
  cfg <- either (throwError . InternalError) pure (Common.operatorConfig integratedBPPConfig.operatorConfig)
  stations <- ExternalCallAPI.getStationList integratedBPPConfig
  operatingHours <- ExternalCallAPI.getOperatingHoursTags integratedBPPConfig
  operatingWindow <- ExternalCallAPI.getOperatingWindow integratedBPPConfig
  case mbJourney of
    -- A stopless discovery search publishes the station roster and prices nothing, so
    -- there is no quote to persist and no origin/destination for an FRFSSearch row.
    Nothing -> pure (discoveryCatalog operator integratedBPPConfig stations operatingHours operatingWindow cfg)
    Just (fromStop, toStop) -> do
      catalog <- journeyCatalog operator merchantId merchantOperatingCity integratedBPPConfig stations operatingHours operatingWindow cfg fromStop toStop
      now <- getCurrentTime
      QuoteStore.persistQuotes
        QuoteStore.SellerQuoteContext
          { operator,
            transactionId,
            merchantId,
            merchantOperatingCityId = merchantOperatingCity.id,
            integratedBPPConfig,
            selfSubscriberId = self.subscriberId,
            selfSubscriberUrl = self.subscriberUrl,
            quantity = cfg.oneWayTicketLimit,
            validTill = addUTCTime (fromIntegral cfg.quoteCache.ttlSeconds) now
          }
        (mkQuotes catalog)
      pure catalog

coordOf :: Double -> Maybe Double
coordOf value = if value == 0.0 then Nothing else Just value

discoveryCatalog :: Text -> DIBC.IntegratedBPPConfig -> [CMRLStationList.Station] -> [(Text, Text)] -> Maybe (UTCTime, UTCTime) -> Common.OperatorConfig -> ACL.SellerCatalog
discoveryCatalog operator integratedBPPConfig stations operatingHours operatingWindow cfg =
  ACL.SellerCatalog
    { providerId = Common.metroProviderId operator,
      providerName = fromMaybe integratedBPPConfig.agencyKey integratedBPPConfig.providerName,
      journeyId = Nothing,
      brandName = cfg.catalog.brandName,
      brandLogoUrl = cfg.catalog.brandLogoUrl,
      cancellationTermsUrl = cfg.cancellation.termsUrl,
      courtJurisdiction = cfg.courtJurisdiction,
      settlementWindow = cfg.settlementWindow,
      buyerFinderFeePercentage = cfg.buyerFinderFeePercentage,
      businessTermsUrl = cfg.businessTermsUrl,
      maxPaidAreaMinutes = cfg.maxPaidAreaMinutes,
      validityLabel = validity.label,
      validityDuration = validity.duration,
      operatingHours,
      operatingWindow,
      fulfillments = map lineFulfillment (linesInOrder stations),
      items = []
    }
  where
    validity = Common.ticketValidity cfg

    linesInOrder =
      groupBy (\a b -> a.lineId == b.lineId)
        . sortOn (\station -> (station.lineId, station.sequenceNo))
        . filter (\station -> not (T.null station.lineId))

    lineFulfillment lineStations =
      ACL.SellerFulfillment
        { fulfillmentId = Common.metroProviderId operator <> "-line-" <> lineId,
          stops = zipWith (mkLineStop (length lineStations)) [1 :: Int ..] lineStations
        }
      where
        lineId = maybe "" (.lineId) (listToMaybe lineStations)

    mkLineStop total position station =
      ACL.SellerStop
        { stopCode = station.stationId,
          stopCanonicalCode = station.code <> "|" <> station.stationId,
          stopName = station.name,
          stopLat = coordOf station.latitude,
          stopLon = coordOf station.longitude,
          stopType = stopTypeAt total position
        }

    stopTypeAt total position
      | position == 1 = SpecEnums.START
      | position == total = SpecEnums.END
      | otherwise = SpecEnums.INTERMEDIATE_STOP

journeyCatalog :: Text -> Id DM.Merchant -> DMOC.MerchantOperatingCity -> DIBC.IntegratedBPPConfig -> [CMRLStationList.Station] -> [(Text, Text)] -> Maybe (UTCTime, UTCTime) -> Common.OperatorConfig -> Text -> Text -> Flow ACL.SellerCatalog
journeyCatalog operator merchantId merchantOperatingCity integratedBPPConfig stations operatingHours operatingWindow cfg fromStop toStop = do
  fromStation <- resolveStation stations fromStop
  toStation <- resolveStation stations toStop
  let priceJourneyType journeyType =
        ExternalCallAPI.getFaresForJourneyType
          Common.sellerRiderId
          merchantId
          merchantOperatingCity.id
          integratedBPPConfig
          (ExternalCallAPI.BasicRouteDetail {routeCode = "", startStopCode = fromStop, endStopCode = toStop, color = Nothing} :| [])
          SpecEnums.METRO
          Nothing
          Nothing
          (Just journeyType.quoteType)
  faresByJourneyType <-
    fmap catMaybes . forM Common.sellerJourneyTypes $ \journeyType ->
      withTryCatch ("frfsSeller:getFares:" <> show journeyType.code) (priceJourneyType journeyType) >>= \case
        Right fares -> pure (Just (journeyType, fares))
        Left err -> do
          Metrics.incrementExternalProviderFailure integratedBPPConfig.agencyKey "getFare" "exception"
          logError $ "frfsSeller:getFares failed for " <> show journeyType.code <> ", dropping it from the catalog: " <> show err
          pure Nothing
  journeyId <-
    Common.journeyIdFromStationNames fromStation.name toStation.name
      & fromMaybeM (InvalidRequest $ "Unresolved station name for " <> fromStop <> " or " <> toStop)
  let validity = Common.ticketValidity cfg
  let items = concatMap (\(journeyType, fares) -> concatMap (mkItems cfg journeyId journeyType) fares) faresByJourneyType
  when (null items) . throwError . InvalidRequest $
    "No fares available for " <> fromStop <> " -> " <> toStop
  pure
    ACL.SellerCatalog
      { providerId = Common.metroProviderId operator,
        providerName = fromMaybe integratedBPPConfig.agencyKey integratedBPPConfig.providerName,
        journeyId = Just journeyId,
        brandName = cfg.catalog.brandName,
        brandLogoUrl = cfg.catalog.brandLogoUrl,
        cancellationTermsUrl = cfg.cancellation.termsUrl,
        courtJurisdiction = cfg.courtJurisdiction,
        settlementWindow = cfg.settlementWindow,
        buyerFinderFeePercentage = cfg.buyerFinderFeePercentage,
        businessTermsUrl = cfg.businessTermsUrl,
        maxPaidAreaMinutes = cfg.maxPaidAreaMinutes,
        validityLabel = validity.label,
        validityDuration = validity.duration,
        operatingHours,
        operatingWindow,
        fulfillments =
          [ ACL.SellerFulfillment
              { fulfillmentId = journeyId,
                stops =
                  [ ACL.SellerStop {stopCode = fromStop, stopCanonicalCode = fromStation.code <> "|" <> fromStation.stationId, stopName = fromStation.name, stopLat = coordOf fromStation.latitude, stopLon = coordOf fromStation.longitude, stopType = SpecEnums.START},
                    ACL.SellerStop {stopCode = toStop, stopCanonicalCode = toStation.code <> "|" <> toStation.stationId, stopName = toStation.name, stopLat = coordOf toStation.latitude, stopLon = coordOf toStation.longitude, stopType = SpecEnums.END}
                  ]
              }
          ],
        items
      }
  where
    resolveStation roster code =
      find (\station -> station.stationId == code || station.code <> "|" <> station.stationId == code) roster
        & fromMaybeM (InvalidRequest $ "Unknown station code: " <> code)

-- | One item per journey type, which is what Go emits: @metro_transformer.go:138-145@ builds
-- the id as @<journeyType>-<stations>@ while iterating FareDetails, a list with no category
-- dimension. Fanning out over categories would hand every item that same id, and QuoteStore
-- keys on the id alone, so the last one written would win.
mkItems :: Common.OperatorConfig -> Text -> Common.SellerJourneyType -> FRFSUtils.FRFSFare -> [ACL.SellerItem]
mkItems cfg journeyId journeyType fare =
  [ ACL.SellerItem
      { itemId = T.toLower (show journeyType.code) <> "-" <> journeyId,
        journeyTypeCode = show journeyType.code,
        itemDescription = journeyType.name,
        maxTicketsPerOrder = Common.maxTicketsPerOrder cfg journeyType.code,
        fareQuoteId = fare.fareQuoteId,
        priceValue = Common.formatPrice (realToFrac category.offeredPrice.amount),
        currency = show category.offeredPrice.currency
      }
    | category <- maybeToList (adultCategory fare)
  ]
  where
    adultCategory f = find (\c -> c.category == ADULT) f.categories <|> listToMaybe f.categories

intentStops :: Spec.SearchReq -> Maybe [Spec.Stop]
intentStops req =
  req.searchReqMessage.searchReqMessageIntent
    >>= (.intentFulfillment)
    >>= (.fulfillmentStops)

originAndDestination :: [Spec.Stop] -> Flow (Text, Text)
originAndDestination stops = do
  fromStop <- stopCode (FRFSUtilsACL.getStartStop stops) & fromMaybeM (InvalidRequest "No START stop with a station code")
  toStop <- stopCode (FRFSUtilsACL.getEndStop stops) & fromMaybeM (InvalidRequest "No END stop with a station code")
  pure (fromStop, toStop)
  where
    stopCode mbStop = mbStop >>= (.stopLocation) >>= (.locationDescriptor) >>= (.descriptorCode)
