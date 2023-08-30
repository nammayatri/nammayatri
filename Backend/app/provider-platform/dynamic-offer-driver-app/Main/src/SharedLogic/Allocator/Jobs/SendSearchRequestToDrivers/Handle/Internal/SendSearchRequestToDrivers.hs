{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.SendSearchRequestToDrivers
  ( sendSearchRequestToDrivers,
  )
where

import qualified Data.Map as M
import qualified Domain.Types.Driver.DriverFlowStatus as DDFS
import qualified Domain.Types.FarePolicy as DFP
import Domain.Types.Merchant.DriverPoolConfig
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchRequest.SearchReqLocation as DLoc
import Domain.Types.SearchRequestForDriver
import qualified Domain.Types.SearchTry as DST
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.CacheFlow
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common (addUTCTime, logInfo)
import qualified Lib.DriverScore as DS
import qualified Lib.DriverScore.Types as DST
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool (getPoolBatchNum)
import SharedLogic.DriverPool
import SharedLogic.GoogleTranslate
import qualified Storage.CachedQueries.BapMetadata as CQSM
import qualified Storage.Queries.Driver.DriverFlowStatus as QDFS
import qualified Storage.Queries.SearchRequestForDriver as QSRD
import Tools.Maps as Maps
import qualified Tools.Notifications as Notify

type LanguageDictionary = M.Map Maps.Language DSR.SearchRequest

sendSearchRequestToDrivers ::
  ( Log m,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    TranslateFlow m r,
    CacheFlow m r,
    EncFlow m r
  ) =>
  DSR.SearchRequest ->
  DST.SearchTry ->
  Maybe DFP.DriverExtraFeeBounds ->
  DriverPoolConfig ->
  [DriverPoolWithActualDistResult] ->
  m ()
sendSearchRequestToDrivers searchReq searchTry driverExtraFeeBounds driverPoolConfig driverPool = do
  logInfo $ "Send search requests to driver pool batch-" <> show driverPool
  bapMetadata <- CQSM.findById (Id searchReq.bapId)
  validTill <- getSearchRequestValidTill
  batchNumber <- getPoolBatchNum searchTry.id
  languageDictionary <- foldM (addLanguageToDictionary searchReq) M.empty driverPool
  DS.driverScoreEventHandler
    DST.OnNewSearchRequestForDrivers
      { driverPool = driverPool,
        merchantId = searchReq.providerId,
        searchReq = searchReq,
        searchTry = searchTry,
        validTill = validTill,
        batchProcessTime = fromIntegral driverPoolConfig.singleBatchProcessTime
      }

  searchRequestsForDrivers <- mapM (buildSearchRequestForDriver batchNumber validTill) driverPool
  let driverPoolZipSearchRequests = zip driverPool searchRequestsForDrivers
  _ <- QSRD.setInactiveBySTId searchTry.id -- inactive previous request by drivers so that they can make new offers.
  _ <- QSRD.createMany searchRequestsForDrivers
  forM_ searchRequestsForDrivers $ \sReqFD -> do
    QDFS.updateStatus sReqFD.driverId DDFS.GOT_SEARCH_REQUEST {requestId = searchTry.id, searchTryId = searchTry.id, validTill = sReqFD.searchRequestValidTill}

  forM_ driverPoolZipSearchRequests $ \(dPoolRes, sReqFD) -> do
    let language = fromMaybe Maps.ENGLISH dPoolRes.driverPoolResult.language
    let translatedSearchReq = fromMaybe searchReq $ M.lookup language languageDictionary
    let entityData = makeSearchRequestForDriverAPIEntity sReqFD translatedSearchReq searchTry bapMetadata dPoolRes.intelligentScores.rideRequestPopupDelayDuration dPoolRes.keepHiddenForSeconds searchTry.vehicleVariant

    Notify.notifyOnNewSearchRequestAvailable searchReq.providerId sReqFD.driverId dPoolRes.driverPoolResult.driverDeviceToken entityData
  where
    getSearchRequestValidTill = do
      now <- getCurrentTime
      let singleBatchProcessTime = fromIntegral driverPoolConfig.singleBatchProcessTime
      return $ singleBatchProcessTime `addUTCTime` now
    buildSearchRequestForDriver ::
      ( MonadFlow m,
        Redis.HedisFlow m r
      ) =>
      Int ->
      UTCTime ->
      DriverPoolWithActualDistResult ->
      m SearchRequestForDriver
    buildSearchRequestForDriver batchNumber validTill dpwRes = do
      guid <- generateGUID
      now <- getCurrentTime
      let dpRes = dpwRes.driverPoolResult
      parallelSearchRequestCount <- Just <$> getValidSearchRequestCount searchReq.providerId dpRes.driverId now
      let searchRequestForDriver =
            SearchRequestForDriver
              { id = guid,
                requestId = searchReq.id,
                searchTryId = searchTry.id,
                startTime = searchTry.startTime,
                merchantId = Just searchReq.providerId,
                searchRequestValidTill = validTill,
                driverId = cast dpRes.driverId,
                vehicleVariant = dpRes.variant,
                actualDistanceToPickup = dpwRes.actualDistanceToPickup,
                straightLineDistanceToPickup = dpRes.distanceToPickup,
                durationToPickup = dpwRes.actualDurationToPickup,
                status = Active,
                lat = Just dpRes.lat,
                lon = Just dpRes.lon,
                createdAt = now,
                response = Nothing,
                driverMinExtraFee = driverExtraFeeBounds <&> (.minFee),
                driverMaxExtraFee = driverExtraFeeBounds <&> (.maxFee),
                rideRequestPopupDelayDuration = dpwRes.intelligentScores.rideRequestPopupDelayDuration,
                isPartOfIntelligentPool = dpwRes.isPartOfIntelligentPool,
                acceptanceRatio = dpwRes.intelligentScores.acceptanceRatio,
                cancellationRatio = dpwRes.intelligentScores.cancellationRatio,
                driverAvailableTime = dpwRes.intelligentScores.availableTime,
                driverSpeed = dpwRes.intelligentScores.driverSpeed,
                keepHiddenForSeconds = dpwRes.keepHiddenForSeconds,
                mode = dpRes.mode,
                goHomeRequestId = dpwRes.goHomeReqId,
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
  DSR.SearchRequest ->
  Maps.Language ->
  m DSR.SearchRequest
translateSearchReq DSR.SearchRequest {..} language = do
  from <- buildTranslatedSearchReqLocation fromLocation (Just language)
  to <- buildTranslatedSearchReqLocation toLocation (Just language)
  pure
    DSR.SearchRequest
      { fromLocation = from,
        toLocation = to,
        ..
      }

addLanguageToDictionary ::
  ( TranslateFlow m r,
    CacheFlow m r,
    EsqDBFlow m r
  ) =>
  DSR.SearchRequest ->
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
