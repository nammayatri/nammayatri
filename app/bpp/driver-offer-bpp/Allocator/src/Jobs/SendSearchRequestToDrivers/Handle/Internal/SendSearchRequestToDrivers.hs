module Jobs.SendSearchRequestToDrivers.Handle.Internal.SendSearchRequestToDrivers
  ( sendSearchRequestToDrivers,
  )
where

import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Utils.Common (logInfo)
import qualified Data.Map as M
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchRequest as DSearchReq
import qualified Domain.Types.SearchRequest.SearchReqLocation as DLoc
import Domain.Types.SearchRequestForDriver
import Environment
import SharedLogic.DriverPool
import SharedLogic.GoogleTranslate
import Storage.CachedQueries.CacheConfig (CacheFlow)
import qualified Storage.Queries.SearchRequestForDriver as QSRD
import Tools.Maps as Maps
import qualified Tools.Notifications as Notify

type LanguageDictionary = M.Map Maps.Language DSearchReq.SearchRequest

sendSearchRequestToDrivers :: DSR.SearchRequest -> Money -> [DriverPoolWithActualDistResult] -> Flow ()
sendSearchRequestToDrivers searchReq baseFare driverPool = do
  logInfo $ "Final Driver Pool " <> show driverPool
  searchRequestsForDrivers <- mapM (buildSearchRequestForDriver searchReq baseFare) driverPool
  languageDictionary <- foldM (addLanguageToDictionary searchReq) M.empty driverPool
  Esq.runTransaction $ do
    QSRD.createMany searchRequestsForDrivers
  let driverPoolZipSearchRequests = zip driverPool searchRequestsForDrivers
  forM_ driverPoolZipSearchRequests $ \(dPoolRes, sReqFD) -> do
    incrementTotalCount sReqFD.driverId
    let language = fromMaybe Maps.ENGLISH dPoolRes.driverPoolResult.language
    let translatedSearchReq = fromMaybe searchReq $ M.lookup language languageDictionary
    let entityData = makeSearchRequestForDriverAPIEntity sReqFD translatedSearchReq
    Notify.notifyOnNewSearchRequestAvailable searchReq.providerId sReqFD.driverId dPoolRes.driverPoolResult.driverDeviceToken entityData
  where
    buildSearchRequestForDriver ::
      (MonadFlow m) =>
      DSearchReq.SearchRequest ->
      Money ->
      DriverPoolWithActualDistResult ->
      m SearchRequestForDriver
    buildSearchRequestForDriver searchRequest baseFare_ dpwRes = do
      guid <- generateGUID
      now <- getCurrentTime
      let dpRes = dpwRes.driverPoolResult
      let searchRequestForDriver =
            SearchRequestForDriver
              { id = guid,
                searchRequestId = searchRequest.id,
                startTime = searchRequest.startTime,
                searchRequestValidTill = searchRequest.validTill,
                driverId = cast dpRes.driverId,
                vehicleVariant = dpRes.variant,
                actualDistanceToPickup = dpwRes.actualDistanceToPickup,
                straightLineDistanceToPickup = dpRes.distanceToPickup,
                durationToPickup = dpwRes.actualDurationToPickup,
                status = Active,
                lat = Just dpRes.lat,
                lon = Just dpRes.lon,
                baseFare = baseFare_,
                createdAt = now,
                response = Nothing,
                ..
              }
      pure searchRequestForDriver

buildTranslatedSearchReqLocation :: (TranslateFlow m r, EsqDBFlow m r, CacheFlow m r) => DLoc.SearchReqLocation -> Maybe Maps.Language -> m DLoc.SearchReqLocation
buildTranslatedSearchReqLocation DLoc.SearchReqLocation {..} mbLanguage = do
  areaRegional <- case mbLanguage of
    Nothing -> return area
    Just lang -> do
      mAreaObj <- translate ENGLISH lang `mapM` area
      let translation = (\areaObj -> listToMaybe areaObj._data.translations) =<< mAreaObj
      return $ (.translatedText) <$> translation
  pure
    DLoc.SearchReqLocation
      { area = areaRegional,
        ..
      }

translateSearchReq ::
  ( TranslateFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  DSearchReq.SearchRequest ->
  Maps.Language ->
  m DSearchReq.SearchRequest
translateSearchReq DSearchReq.SearchRequest {..} language = do
  from <- buildTranslatedSearchReqLocation fromLocation (Just language)
  to <- buildTranslatedSearchReqLocation toLocation (Just language)
  pure
    DSearchReq.SearchRequest
      { fromLocation = from,
        toLocation = to,
        ..
      }

addLanguageToDictionary ::
  ( TranslateFlow m r,
    CacheFlow m r,
    EsqDBFlow m r
  ) =>
  DSearchReq.SearchRequest ->
  LanguageDictionary ->
  DriverPoolWithActualDistResult ->
  m LanguageDictionary
addLanguageToDictionary searchReq dict dPoolRes = do
  let language = fromMaybe Maps.ENGLISH dPoolRes.driverPoolResult.language
  if isJust $ M.lookup language dict
    then return dict
    else do
      translatedSearchReq <- translateSearchReq searchReq language
      pure $ M.insert language translatedSearchReq dict
