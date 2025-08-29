module Storage.CachedQueries.Driver.GoHomeRequest where

import Control.Monad
import Data.Text (pack)
import Domain.Action.UI.DriverGoHomeRequest
import Domain.Types.DriverGoHomeRequest as DDGR
import Domain.Types.DriverHomeLocation as DDHL
import Domain.Types.GoHomeConfig
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBFlow)
import Kernel.Storage.Hedis (withCrossAppRedis)
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.App (MonadFlow)
import Kernel.Types.CacheFlow
import Kernel.Types.Common (MonadTime (getCurrentTime), generateGUID)
import Kernel.Types.Id (Id, cast)
import Kernel.Types.SlidingWindowCounters (PeriodType (Days))
import Kernel.Utils.Common (addUTCTime, fromMaybeM, getLocalCurrentTime)
import Kernel.Utils.Logging (logDebug)
import Kernel.Utils.SlidingWindowCounters (incrementPeriod)
import qualified Storage.Cac.GoHomeConfig as CGHC
import Storage.Cac.TransporterConfig as SCTC
import Storage.Queries.DriverGoHomeRequest as QDGR
import Storage.Queries.Ride as Ride
import Tools.Error (GenericError (..))
import Utils.Common.Cac.KeyNameConstants

makeGoHomeReqKey :: Id DP.Driver -> Text
makeGoHomeReqKey = pack . ("CachedQueries:GoHomeRequest-driverId:" <>) . show

getDriverGoHomeRequestInfo :: (CacheFlow m r, MonadFlow m, EsqDBFlow m r) => Id DP.Driver -> Id DMOC.MerchantOperatingCity -> Maybe GoHomeConfig -> m CachedGoHomeRequest
getDriverGoHomeRequestInfo driverId merchantOpCityId goHomeCfg = do
  ghCfg <- maybe (CGHC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast driverId)))) return goHomeCfg
  let initCnt = ghCfg.startCnt
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let ghkey = makeGoHomeReqKey driverId
  currTime <- getLocalCurrentTime =<< ((SCTC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast driverId))) >>= fromMaybeM (InternalError "Transporter config for timezone not found")) <&> (.timeDiffFromUtc))
  withCrossAppRedis $
    Hedis.safeGet ghkey >>= \case
      Just ghrData -> do
        if maybe False (\validTill -> validTill > currTime || ghrData.isOnRide) ghrData.validTill
          then return ghrData
          else checkInvalidReqData ghrData currTime ghkey driverId merchantOpCityId ghCfg expTime
      Nothing -> do
        logDebug $ "Setting new CachedGoHomeRequest data as old data not found for driverId :" <> show driverId
        mbDghr <- QDGR.findActive driverId
        whenJust mbDghr $ \dghr -> do
          succRide <- Ride.findCompletedRideByGHRId dghr.id
          finishWithStatus (bool DDGR.FAILED DDGR.SUCCESS (isJust succRide)) (bool Nothing (Just False) (isJust succRide)) dghr.id
        withCrossAppRedis $ Hedis.setExp ghkey (templateGoHomeData Nothing initCnt Nothing Nothing False Nothing currTime) expTime
        return $ templateGoHomeData Nothing initCnt Nothing Nothing False Nothing currTime

checkInvalidReqData :: (CacheFlow m r, MonadFlow m, EsqDBFlow m r) => CachedGoHomeRequest -> UTCTime -> Text -> Id DP.Driver -> Id DMOC.MerchantOperatingCity -> GoHomeConfig -> Int -> m CachedGoHomeRequest
checkInvalidReqData ghrData currTime ghkey driverId merchantOpCityId goHomeCfg expTime = do
  let initCnt = goHomeCfg.startCnt
  if ghrData.goHomeReferenceTime > currTime
    then case ghrData.validTill of
      Just _ -> do
        ghrId <- fromMaybeM (InternalError "GoHomeRequestId not present but valid Till set") ghrData.driverGoHomeRequestId
        succRide <- Ride.findCompletedRideByGHRId ghrId
        if isJust succRide
          then do
            deactivateDriverGoHomeRequest merchantOpCityId driverId SUCCESS ghrData (Just False)
            return (templateGoHomeData Nothing (ghrData.cnt - 1) Nothing Nothing False (Just ghrData.goHomeReferenceTime) currTime)
          else do
            deactivateDriverGoHomeRequest merchantOpCityId driverId FAILED ghrData Nothing
            return (templateGoHomeData Nothing ghrData.cnt Nothing Nothing False (Just ghrData.goHomeReferenceTime) currTime)
      Nothing -> do
        return ghrData
    else do
      logDebug $ "Setting new CachedGoHomeRequest data as old data is expired for driverId :" <> show driverId
      whenJust (ghrData.driverGoHomeRequestId) $ \id -> do
        succRide <- Ride.findCompletedRideByGHRId id
        QDGR.finishWithStatus (bool DDGR.FAILED DDGR.SUCCESS (isJust succRide)) (bool Nothing (Just False) (isJust succRide)) id
      withCrossAppRedis $ Hedis.setExp ghkey (templateGoHomeData Nothing initCnt Nothing Nothing False Nothing currTime) expTime
      return $ templateGoHomeData Nothing initCnt Nothing Nothing False Nothing currTime

templateGoHomeData :: Maybe DriverGoHomeRequestStatus -> Int -> Maybe UTCTime -> Maybe (Id DriverGoHomeRequest) -> Bool -> Maybe UTCTime -> UTCTime -> CachedGoHomeRequest
templateGoHomeData stat count vTill ghrId isOnRde ghValidityTime currTime =
  CachedGoHomeRequest
    { status = stat,
      cnt = count,
      validTill = vTill,
      driverGoHomeRequestId = ghrId,
      isOnRide = isOnRde,
      goHomeReferenceTime = fromMaybe (incrementPeriod Days currTime) ghValidityTime
    }

activateDriverGoHomeRequest :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Id DP.Driver -> DDHL.DriverHomeLocation -> GoHomeConfig -> CachedGoHomeRequest -> m ()
activateDriverGoHomeRequest merchantId merchantOpCityId driverId driverHomeLoc goHomeConfig ghInfo = do
  let ghKey = makeGoHomeReqKey driverId
  let activeTime = goHomeConfig.activeTime
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  currTime <- getLocalCurrentTime =<< ((SCTC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast driverId))) >>= fromMaybeM (InternalError "Transporter config for timezone not found")) <&> (.timeDiffFromUtc))
  guId <- generateGUID
  _ <- QDGR.create =<< buildDriverGoHomeRequest guId driverHomeLoc
  let vTill = addUTCTime (fromIntegral activeTime) currTime
  withCrossAppRedis $ Hedis.setExp ghKey (templateGoHomeData (Just DDGR.ACTIVE) ghInfo.cnt (Just vTill) (Just guId) False (Just ghInfo.goHomeReferenceTime) currTime) expTime
  where
    buildDriverGoHomeRequest guId driverHomeLocation = do
      let id = guId
      now <- getCurrentTime
      return $
        DDGR.DriverGoHomeRequest
          { lat = driverHomeLocation.lat,
            lon = driverHomeLocation.lon,
            status = DDGR.ACTIVE,
            numCancellation = 0,
            mbReachedHome = Nothing,
            createdAt = now,
            updatedAt = now,
            merchantId = Just merchantId,
            merchantOperatingCityId = Just merchantOpCityId,
            ..
          }

deactivateDriverGoHomeRequest :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> Id DP.Driver -> DDGR.DriverGoHomeRequestStatus -> CachedGoHomeRequest -> Maybe Bool -> m ()
deactivateDriverGoHomeRequest merchantOpCityId driverId stat ghInfo mbReachedHome = do
  currTime <- getLocalCurrentTime =<< ((SCTC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast driverId))) >>= fromMaybeM (InternalError "Transporter config for timezone not found")) <&> (.timeDiffFromUtc))
  let ghKey = makeGoHomeReqKey driverId
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  driverGoHomeReqId <- fromMaybeM (InternalError "Could not Find DriverGoHomeRequestId") ghInfo.driverGoHomeRequestId
  QDGR.finishWithStatus stat mbReachedHome driverGoHomeReqId
  withCrossAppRedis $ Hedis.setExp ghKey (templateGoHomeData Nothing (bool ghInfo.cnt (ghInfo.cnt - 1) (stat == DDGR.SUCCESS)) Nothing Nothing False (Just ghInfo.goHomeReferenceTime) currTime) expTime

resetDriverGoHomeRequest :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> Id DP.Driver -> GoHomeConfig -> CachedGoHomeRequest -> m ()
resetDriverGoHomeRequest merchantOpCityId driverId goHomeConfig ghInfo = do
  currTime <- getLocalCurrentTime =<< ((SCTC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast driverId))) >>= fromMaybeM (InternalError "Transporter config for timezone not found")) <&> (.timeDiffFromUtc))
  let ghKey = makeGoHomeReqKey driverId
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  withCrossAppRedis $ Hedis.setExp ghKey (templateGoHomeData ghInfo.status ghInfo.cnt (Just $ addUTCTime (fromIntegral goHomeConfig.activeTime) currTime) ghInfo.driverGoHomeRequestId False (Just ghInfo.goHomeReferenceTime) currTime) expTime

increaseDriverGoHomeRequestCount :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> Id DP.Driver -> m ()
increaseDriverGoHomeRequestCount merchantOpCityId driverId = do
  currTime <- getLocalCurrentTime =<< ((SCTC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast driverId))) >>= fromMaybeM (InternalError "Transporter config for timezone not found")) <&> (.timeDiffFromUtc))
  let ghKey = makeGoHomeReqKey driverId
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  ghInfo <- getDriverGoHomeRequestInfo driverId merchantOpCityId Nothing
  withCrossAppRedis $ Hedis.setExp ghKey (templateGoHomeData ghInfo.status (ghInfo.cnt + 1) ghInfo.validTill ghInfo.driverGoHomeRequestId ghInfo.isOnRide (Just ghInfo.goHomeReferenceTime) currTime) expTime

setDriverGoHomeIsOnRideStatus :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id DP.Driver -> Id DMOC.MerchantOperatingCity -> Bool -> m (Maybe (Id DriverGoHomeRequest))
setDriverGoHomeIsOnRideStatus driverId merchantOpCityId status = do
  ghCfg <- CGHC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast driverId)))
  if ghCfg.enableGoHome
    then do
      currTime <- getLocalCurrentTime =<< ((SCTC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast driverId))) >>= fromMaybeM (InternalError "Transporter config for timezone not found")) <&> (.timeDiffFromUtc))
      let ghKey = makeGoHomeReqKey driverId
      expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
      ghInfo <- getDriverGoHomeRequestInfo driverId merchantOpCityId (Just ghCfg)
      when (ghInfo.status == Just DDGR.ACTIVE) $ withCrossAppRedis $ Hedis.setExp ghKey (templateGoHomeData ghInfo.status ghInfo.cnt ghInfo.validTill ghInfo.driverGoHomeRequestId status (Just ghInfo.goHomeReferenceTime) currTime) expTime
      return ghInfo.driverGoHomeRequestId
    else return Nothing
