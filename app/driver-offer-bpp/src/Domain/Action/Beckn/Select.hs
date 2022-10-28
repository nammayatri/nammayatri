{-# OPTIONS_GHC -Wno-unused-matches #-}

module Domain.Action.Beckn.Select where

import qualified Beckn.External.GoogleTranslate.Types as GoogleTranslate
import qualified Beckn.External.Maps.Google as GoogleMaps
import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Utils.Common (fromMaybeM, logDebug)
import qualified Data.Map as M
import Data.Time.Clock (addUTCTime)
import qualified Domain.Types.Organization as DOrg
import qualified Domain.Types.SearchRequest as DSearchReq
import qualified Domain.Types.SearchRequest.SearchReqLocation as DLoc
import Domain.Types.SearchRequestForDriver
import Domain.Types.Vehicle.Variant (Variant)
import Environment
import SharedLogic.DriverPool
import SharedLogic.FareCalculator
import SharedLogic.GoogleTranslate
import qualified Storage.CachedQueries.FarePolicy as FarePolicyS
import Storage.Queries.Person
import qualified Storage.Queries.SearchRequest as QSReq
import qualified Storage.Queries.SearchRequestForDriver as QSRD
import Tools.Error (FarePolicyError (NoFarePolicy))
import Tools.Metrics (CoreMetrics)
import qualified Tools.Notifications as Notify

data DSelectReq = DSelectReq
  { messageId :: Text,
    transactionId :: Maybe Text,
    bapId :: Text,
    bapUri :: BaseUrl,
    pickupLocation :: DLoc.SearchReqLocationAPIEntity,
    pickupTime :: UTCTime,
    dropLocation :: DLoc.SearchReqLocationAPIEntity,
    variant :: Variant
  }

type LanguageDictionary = M.Map GoogleMaps.Language DSearchReq.SearchRequest

handler :: Text -> Id DOrg.Organization -> DSelectReq -> Flow ()
handler sessiontoken orgId sReq = do
  fromLocation <- buildSearchReqLocation sReq.pickupLocation
  toLocation <- buildSearchReqLocation sReq.dropLocation
  farePolicy <- FarePolicyS.findByOrgIdAndVariant orgId sReq.variant >>= fromMaybeM NoFarePolicy
  driverPool <- calculateDriverPool (Just sReq.variant) fromLocation orgId False True

  distRes <- GoogleMaps.getDistance (Just GoogleMaps.CAR) fromLocation toLocation
  let distance = distRes.distance
  let duration = distRes.duration
  fareParams <- calculateFare orgId farePolicy distance sReq.pickupTime Nothing
  searchReq <- buildSearchRequest fromLocation toLocation orgId sReq distance duration
  let baseFare = fareSum fareParams
  logDebug $
    "search request id=" <> show searchReq.id
      <> "; estimated distance = "
      <> show distance
      <> "; estimated base fare:"
      <> show baseFare
  searchRequestsForDrivers <- mapM (buildSearchRequestForDriver searchReq baseFare) driverPool
  languageDictionary <- foldM (addLanguageToDictionary sessiontoken searchReq) M.empty driverPool
  Esq.runTransaction $ do
    QSReq.create searchReq
    mapM_ QSRD.create searchRequestsForDrivers
  let driverPoolZipSearchRequests = zip driverPool searchRequestsForDrivers
  forM_ driverPoolZipSearchRequests $ \(dPoolRes, sReqFD) ->
    when (not dPoolRes.origin.onRide) $ do
      let language = fromMaybe GoogleMaps.ENGLISH dPoolRes.origin.language
      let translatedSearchReq = fromMaybe searchReq $ M.lookup language languageDictionary
      let entityData = makeSearchRequestForDriverAPIEntity sReqFD translatedSearchReq
      Notify.notifyOnNewSearchRequestAvailable sReqFD.driverId dPoolRes.origin.driverDeviceToken entityData
  where
    buildSearchRequestForDriver ::
      (MonadFlow m) =>
      DSearchReq.SearchRequest ->
      Money ->
      GoogleMaps.GetDistanceResult DriverPoolResult GoogleMaps.LatLong ->
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
  Id DOrg.Organization ->
  DSelectReq ->
  Meters ->
  Seconds ->
  m DSearchReq.SearchRequest
buildSearchRequest from to orgId sReq distance duration = do
  id_ <- Id <$> generateGUID
  createdAt_ <- getCurrentTime
  searchRequestExpirationSeconds <- asks (.searchRequestExpirationSeconds)
  let validTill_ = searchRequestExpirationSeconds `addUTCTime` createdAt_
  pure
    DSearchReq.SearchRequest
      { id = id_,
        transactionId = fromMaybe "" sReq.transactionId,
        messageId = sReq.messageId,
        startTime = sReq.pickupTime,
        validTill = validTill_,
        providerId = orgId,
        fromLocation = from,
        toLocation = to,
        bapId = sReq.bapId,
        bapUri = sReq.bapUri,
        estimatedDistance = distance,
        estimatedDuration = duration,
        createdAt = createdAt_,
        vehicleVariant = sReq.variant
      }

buildSearchReqLocation :: (MonadGuid m, MonadTime m) => DLoc.SearchReqLocationAPIEntity -> m DLoc.SearchReqLocation
buildSearchReqLocation DLoc.SearchReqLocationAPIEntity {..} = do
  id <- Id <$> generateGUID
  now <- getCurrentTime
  let createdAt = now
      updatedAt = now
  pure DLoc.SearchReqLocation {..}

translateSearchReq ::
  ( MonadFlow m,
    GoogleTranslate.HasGoogleTranslate m r,
    CoreMetrics m
  ) =>
  Text ->
  DSearchReq.SearchRequest ->
  GoogleMaps.Language ->
  m DSearchReq.SearchRequest
translateSearchReq sessiontoken defaultSearchReq@DSearchReq.SearchRequest {..} language = do
  from <- buildLocWithAddr sessiontoken defaultSearchReq.fromLocation language
  to <- buildLocWithAddr sessiontoken defaultSearchReq.toLocation language
  pure
    DSearchReq.SearchRequest
      { fromLocation = from,
        toLocation = to,
        ..
      }

buildLocWithAddr ::
  ( MonadFlow m,
    GoogleTranslate.HasGoogleTranslate m r,
    CoreMetrics m
  ) =>
  Text ->
  DLoc.SearchReqLocation ->
  GoogleMaps.Language ->
  m DLoc.SearchReqLocation
buildLocWithAddr sessiontoken searchReqLoc@DLoc.SearchReqLocation {..} language = do
  mAreaObj <- translate GoogleMaps.ENGLISH language `mapM` searchReqLoc.area
  let translation = (\areaObj -> listToMaybe areaObj._data.translations) =<< mAreaObj
  let areaRegional = (.translatedText) <$> translation
  buildSearchReqLocation (buildSearchReqLocationAPIEntity searchReqLoc areaRegional)

buildSearchReqLocationAPIEntity :: DLoc.SearchReqLocation -> Maybe Text -> DLoc.SearchReqLocationAPIEntity
buildSearchReqLocationAPIEntity DLoc.SearchReqLocation {..} areaRegional = DLoc.SearchReqLocationAPIEntity {area = areaRegional, ..}

addLanguageToDictionary ::
  ( MonadFlow m,
    GoogleTranslate.HasGoogleTranslate m r,
    CoreMetrics m
  ) =>
  Text ->
  DSearchReq.SearchRequest ->
  LanguageDictionary ->
  GoogleMaps.GetDistanceResult DriverPoolResult GoogleMaps.LatLong ->
  m LanguageDictionary
addLanguageToDictionary sessiontoken searchReq dict dPoolRes = do
  let language = fromMaybe GoogleMaps.ENGLISH dPoolRes.origin.language
  if isJust $ M.lookup language dict
    then return dict
    else do
      translatedSearchReq <- translateSearchReq sessiontoken searchReq language
      pure $ M.insert language translatedSearchReq dict
