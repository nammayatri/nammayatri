{-# OPTIONS_GHC -Wno-unused-matches #-}

module Domain.Action.Beckn.Select where

import qualified Beckn.External.GoogleTranslate.Types as GoogleTranslate
import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Utils.Common (fromMaybeM, logDebug, logInfo)
import qualified Data.Map as M
import Data.Time.Clock (addUTCTime)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.SearchRequest as DSearchReq
import qualified Domain.Types.SearchRequest.SearchReqLocation as DLoc
import Domain.Types.SearchRequestForDriver
import Domain.Types.Vehicle.Variant (Variant)
import Environment
import qualified SharedLogic.CacheDistance as CD
import SharedLogic.DriverPool
import SharedLogic.FareCalculator
import SharedLogic.GoogleMaps
import Storage.CachedQueries.CacheConfig (CacheFlow)
import qualified Storage.CachedQueries.FarePolicy as FarePolicyS
import Storage.Queries.Person
import qualified Storage.Queries.SearchRequest as QSReq
import qualified Storage.Queries.SearchRequestForDriver as QSRD
import Tools.Error (FarePolicyError (NoFarePolicy))
import Tools.Maps as Maps
import Tools.Metrics (CoreMetrics)
import qualified Tools.Notifications as Notify

data DSelectReq = DSelectReq
  { messageId :: Text,
    transactionId :: Text,
    bapId :: Text,
    bapUri :: BaseUrl,
    pickupLocation :: LatLong,
    pickupTime :: UTCTime,
    dropLocation :: LatLong,
    variant :: Variant
  }

type LanguageDictionary = M.Map Maps.Language DSearchReq.SearchRequest

handler :: Id DM.Merchant -> DSelectReq -> Flow ()
handler merchantId sReq = do
  sessiontoken <- generateGUIDText
  fromLocation <- buildSearchReqLocation merchantId sessiontoken sReq.pickupLocation Nothing
  toLocation <- buildSearchReqLocation merchantId sessiontoken sReq.dropLocation Nothing
  farePolicy <- FarePolicyS.findByMerchantIdAndVariant merchantId sReq.variant >>= fromMaybeM NoFarePolicy
  driverPool <- calculateDriverPool' (Just sReq.variant) fromLocation
  logInfo $ "Final Driver Pool " <> show driverPool
  mbDistRes <- CD.getCacheDistance sReq.transactionId
  logInfo $ "Fetching cached distance and duration" <> show mbDistRes
  (distance, duration) <-
    case mbDistRes of
      Nothing -> do
        res <-
          Maps.getDistance merchantId $
            Maps.GetDistanceReq
              { origin = fromLocation,
                destination = toLocation,
                travelMode = Just Maps.CAR
              }
        pure (res.distance, res.duration)
      Just distRes -> pure distRes
  fareParams <- calculateFare merchantId farePolicy distance sReq.pickupTime Nothing
  searchReq <- buildSearchRequest fromLocation toLocation merchantId sReq distance duration
  let baseFare = fareSum fareParams
  logDebug $
    "search request id=" <> show searchReq.id
      <> "; estimated distance = "
      <> show distance
      <> "; estimated base fare:"
      <> show baseFare
  searchRequestsForDrivers <- mapM (buildSearchRequestForDriver searchReq baseFare) driverPool
  languageDictionary <- foldM (addLanguageToDictionary merchantId sessiontoken searchReq) M.empty driverPool
  Esq.runTransaction $ do
    QSReq.create searchReq
    mapM_ QSRD.create searchRequestsForDrivers
  let driverPoolZipSearchRequests = zip driverPool searchRequestsForDrivers
  forM_ driverPoolZipSearchRequests $ \(dPoolRes, sReqFD) -> do
    incrementTotalCount sReqFD.driverId
    let language = fromMaybe Maps.ENGLISH dPoolRes.origin.language
    let translatedSearchReq = fromMaybe searchReq $ M.lookup language languageDictionary
    let entityData = makeSearchRequestForDriverAPIEntity sReqFD translatedSearchReq
    Notify.notifyOnNewSearchRequestAvailable sReqFD.driverId dPoolRes.origin.driverDeviceToken entityData
  where
    calculateDriverPool' :: Maybe Variant -> DLoc.SearchReqLocation -> Flow [Maps.GetDistanceResp DriverPoolResult Maps.LatLong]
    calculateDriverPool' mbVariant fromLocation = do
      driverPool' <- calculateDriverPool mbVariant fromLocation merchantId True True
      logInfo $ "All nearby available drivers " <> show driverPool'
      mdriverPoolLimit <- asks (.driverPoolLimit)
      useIntelligentAllocation <- asks (.useIntelligentAllocation)
      case mdriverPoolLimit of
        Just n ->
          if useIntelligentAllocation
            then intelligentPoolSelection driverPool' n
            else randomizeAndLimitSelection driverPool' n
        Nothing -> pure driverPool'

    buildSearchRequestForDriver ::
      (MonadFlow m) =>
      DSearchReq.SearchRequest ->
      Money ->
      Maps.GetDistanceResp DriverPoolResult Maps.LatLong ->
      m SearchRequestForDriver
    buildSearchRequestForDriver searchRequest baseFare_ gdRes = do
      guid <- generateGUID
      now <- getCurrentTime
      let driver = gdRes.origin
      let searchRequestForDriver =
            SearchRequestForDriver
              { id = guid,
                searchRequestId = searchRequest.id,
                startTime = searchRequest.startTime,
                searchRequestValidTill = searchRequest.validTill,
                driverId = cast driver.driverId,
                vehicleVariant = driver.vehicle.variant,
                distanceToPickup = gdRes.distance,
                durationToPickup = gdRes.duration,
                status = Active,
                lat = Just driver.lat,
                lon = Just driver.lon,
                baseFare = baseFare_,
                createdAt = now,
                ..
              }
      pure searchRequestForDriver

buildSearchRequest ::
  ( MonadTime m,
    MonadGuid m,
    MonadReader r m,
    HasField "searchRequestExpirationSeconds" r NominalDiffTime
  ) =>
  DLoc.SearchReqLocation ->
  DLoc.SearchReqLocation ->
  Id DM.Merchant ->
  DSelectReq ->
  Meters ->
  Seconds ->
  m DSearchReq.SearchRequest
buildSearchRequest from to merchantId sReq distance duration = do
  id_ <- Id <$> generateGUID
  createdAt_ <- getCurrentTime
  searchRequestExpirationSeconds <- asks (.searchRequestExpirationSeconds)
  let validTill_ = searchRequestExpirationSeconds `addUTCTime` createdAt_
  pure
    DSearchReq.SearchRequest
      { id = id_,
        transactionId = sReq.transactionId,
        messageId = sReq.messageId,
        startTime = sReq.pickupTime,
        validTill = validTill_,
        providerId = merchantId,
        fromLocation = from,
        toLocation = to,
        bapId = sReq.bapId,
        bapUri = sReq.bapUri,
        estimatedDistance = distance,
        estimatedDuration = duration,
        createdAt = createdAt_,
        vehicleVariant = sReq.variant
      }

buildSearchReqLocation :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r, CoreMetrics m) => Id DM.Merchant -> Text -> LatLong -> Maybe Maps.Language -> m DLoc.SearchReqLocation
buildSearchReqLocation merchantId sessionToken latLong@Maps.LatLong {..} language = do
  pickupRes <-
    Maps.getPlaceName merchantId $
      Maps.GetPlaceNameReq
        { getBy = Maps.ByLatLong latLong,
          sessionToken = Just sessionToken,
          language = language
        }
  Address {..} <- mkLocation pickupRes
  id <- Id <$> generateGUID
  now <- getCurrentTime
  let createdAt = now
      updatedAt = now
  pure DLoc.SearchReqLocation {..}

translateSearchReq ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    GoogleTranslate.HasGoogleTranslate m r,
    CoreMetrics m
  ) =>
  Id DM.Merchant ->
  Text ->
  DSearchReq.SearchRequest ->
  Maps.Language ->
  m DSearchReq.SearchRequest
translateSearchReq orgId sessiontoken defaultSearchReq@DSearchReq.SearchRequest {..} language = do
  from <- buildSearchReqLocation orgId sessiontoken (getCoordinates fromLocation) (Just language)
  to <- buildSearchReqLocation orgId sessiontoken (getCoordinates toLocation) (Just language)
  pure
    DSearchReq.SearchRequest
      { fromLocation = from,
        toLocation = to,
        ..
      }

addLanguageToDictionary ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    GoogleTranslate.HasGoogleTranslate m r,
    CoreMetrics m
  ) =>
  Id DM.Merchant ->
  Text ->
  DSearchReq.SearchRequest ->
  LanguageDictionary ->
  Maps.GetDistanceResp DriverPoolResult Maps.LatLong ->
  m LanguageDictionary
addLanguageToDictionary orgId sessiontoken searchReq dict dPoolRes = do
  let language = fromMaybe Maps.ENGLISH dPoolRes.origin.language
  if isJust $ M.lookup language dict
    then return dict
    else do
      translatedSearchReq <- translateSearchReq orgId sessiontoken searchReq language
      pure $ M.insert language translatedSearchReq dict
