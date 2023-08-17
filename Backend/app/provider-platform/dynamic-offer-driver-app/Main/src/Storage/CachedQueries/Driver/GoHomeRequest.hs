{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Storage.CachedQueries.Driver.GoHomeRequest where

import Control.Monad
import Data.Text (pack)
import Domain.Types.Driver.GoHomeFeature.DriverGoHomeRequest as DDGR
import Domain.Types.Driver.GoHomeFeature.DriverHomeLocation as DDHL
import Domain.Types.Merchant
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBFlow)
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.App (MonadFlow)
import Kernel.Types.Common (MonadTime (getCurrentTime), generateGUID)
import Kernel.Types.Id (Id)
import Kernel.Types.SlidingWindowCounters (PeriodType (Days))
import Kernel.Utils.Common (addUTCTime, fromMaybeM, getLocalCurrentTime)
import Kernel.Utils.Logging (logDebug)
import Kernel.Utils.SlidingWindowCounters (incrementPeriod)
import Storage.CachedQueries.CacheConfig (CacheFlow)
import qualified Storage.CachedQueries.GoHomeConfig as CQGHC
import Storage.CachedQueries.Merchant.TransporterConfig as CQTC
import Storage.Queries.Driver.GoHomeFeature.DriverGoHomeRequest as BeamDHR
import Storage.Queries.Ride as Ride
import Tools.Error (GenericError (..))

makeGoHomeReqKey :: Id DP.Driver -> Text
makeGoHomeReqKey = pack . ("CachedQueries:GoHomeRequest-driverId:" <>) . show

getDriverGoHomeRequestInfo :: (CacheFlow m r, MonadFlow m, EsqDBFlow m r) => Id DP.Driver -> Id Merchant -> m CachedGoHomeRequest
getDriverGoHomeRequestInfo driverId merchantId = do
  initCnt <- CQGHC.findByMerchantId merchantId <&> (.startCnt)
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let ghkey = makeGoHomeReqKey driverId
  currTime <- getLocalCurrentTime =<< ((CQTC.findByMerchantId merchantId >>= fromMaybeM (InternalError "Transporter config for timezone not found")) <&> (.timeDiffFromUtc))
  Hedis.safeGet ghkey >>= \case
    Just ghrData -> do
      if fromMaybe False $ liftM2 (||) (ghrData.validTill <&> (> currTime)) (pure ghrData.isOnRide)
        then return ghrData
        else checkInvalidReqData ghrData currTime ghkey driverId merchantId
    Nothing -> do
      Hedis.setExp ghkey (templateGoHomeData Nothing initCnt Nothing Nothing False Nothing currTime) expTime
      return $ templateGoHomeData Nothing initCnt Nothing Nothing False Nothing currTime

checkInvalidReqData :: (CacheFlow m r, MonadFlow m, EsqDBFlow m r) => CachedGoHomeRequest -> UTCTime -> Text -> Id DP.Driver -> Id Merchant -> m CachedGoHomeRequest
checkInvalidReqData ghrData currTime ghkey driverId merchantId = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  initCnt <- CQGHC.findByMerchantId merchantId <&> (.startCnt)
  if ghrData.expiryTime > currTime
    then case ghrData.validTill of
      Just _ -> do
        ghrId <- fromMaybeM (InternalError "GoHomeRequestId not present but valid Till set") ghrData.driverGoHomeRequestId
        succRide <- Ride.findSuccRideByGoHomeRequestId ghrId
        if isJust succRide
          then do
            deactivateDriverGoHomeRequest merchantId driverId (Just SUCCESS)
            return (templateGoHomeData Nothing (ghrData.cnt - 1) Nothing Nothing False (Just ghrData.expiryTime) currTime)
          else do
            logDebug "Driver Deactivating GoHomeRequest"
            deactivateDriverGoHomeRequest merchantId driverId (Just FAILED)
            return (templateGoHomeData Nothing ghrData.cnt Nothing Nothing False (Just ghrData.expiryTime) currTime)
      Nothing -> do
        return ghrData
    else do
      whenJust (ghrData.driverGoHomeRequestId) BeamDHR.finishWithFailure --Failing it as a default case as it's already a new day and old count does not matter.
      Hedis.setExp ghkey (templateGoHomeData Nothing initCnt Nothing Nothing False Nothing currTime) expTime
      return $ templateGoHomeData Nothing initCnt Nothing Nothing False Nothing currTime

templateGoHomeData :: Maybe DriverGoHomeRequestStatus -> Int -> Maybe UTCTime -> Maybe (Id DriverGoHomeRequest) -> Bool -> Maybe UTCTime -> UTCTime -> CachedGoHomeRequest
templateGoHomeData stat count vTill ghrId isOnRde expiryTime currTime =
  CachedGoHomeRequest
    { status = stat,
      cnt = count,
      validTill = vTill,
      driverGoHomeRequestId = ghrId,
      isOnRide = isOnRde,
      expiryTime = fromMaybe (incrementPeriod Days currTime) expiryTime
    }

activateDriverGoHomeRequest :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Merchant -> Id DP.Driver -> DDHL.DriverHomeLocation -> m ()
activateDriverGoHomeRequest merchantId driverId driverHomeLoc = do
  let ghKey = makeGoHomeReqKey driverId
  activeTime <- CQGHC.findByMerchantId merchantId <&> (.activeTime)
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  currTime <- getLocalCurrentTime =<< ((CQTC.findByMerchantId merchantId >>= fromMaybeM (InternalError "Transporter config for timezone not found")) <&> (.timeDiffFromUtc))
  guId <- generateGUID
  _ <- BeamDHR.create =<< buildDriverGoHomeRequest guId driverHomeLoc
  ghInfo <- getDriverGoHomeRequestInfo driverId merchantId
  let vTill = addUTCTime (fromIntegral activeTime) currTime -- config
  Hedis.setExp ghKey (templateGoHomeData (Just DDGR.ACTIVE) ghInfo.cnt (Just vTill) (Just guId) False (Just ghInfo.expiryTime) currTime) expTime
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
            createdAt = now,
            updatedAt = now,
            ..
          }

deactivateDriverGoHomeRequest :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Merchant -> Id DP.Driver -> Maybe DDGR.DriverGoHomeRequestStatus -> m ()
deactivateDriverGoHomeRequest merchantId driverId mbStatus = do
  currTime <- getLocalCurrentTime =<< ((CQTC.findByMerchantId merchantId >>= fromMaybeM (InternalError "Transporter config for timezone not found")) <&> (.timeDiffFromUtc))
  let ghKey = makeGoHomeReqKey driverId
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  ghInfo <- getDriverGoHomeRequestInfo driverId merchantId
  driverGoHomeReqId <- fromMaybeM (InternalError "Could not Find DriverGoHomeRequestId") ghInfo.driverGoHomeRequestId
  case mbStatus of
    Just SUCCESS -> do
      BeamDHR.finishSuccessfully driverGoHomeReqId
      Hedis.setExp ghKey (templateGoHomeData Nothing (ghInfo.cnt - 1) Nothing Nothing False (Just ghInfo.expiryTime) currTime) expTime
    Just FAILED -> do
      logDebug "Here:- inside deactivate driver go home request"
      BeamDHR.finishWithFailure driverGoHomeReqId
      Hedis.setExp ghKey (templateGoHomeData Nothing ghInfo.cnt Nothing Nothing False (Just ghInfo.expiryTime) currTime) expTime
    Nothing -> do
      succRide <- Ride.findSuccRideByGoHomeRequestId driverGoHomeReqId
      logDebug $ "Seccess ride stat :" <> show succRide
      if isJust succRide
        then do
          BeamDHR.finishSuccessfully driverGoHomeReqId
          Hedis.setExp ghKey (templateGoHomeData Nothing (ghInfo.cnt - 1) Nothing Nothing False (Just ghInfo.expiryTime) currTime) expTime
        else do
          BeamDHR.finishWithFailure driverGoHomeReqId
          Hedis.setExp ghKey (templateGoHomeData Nothing ghInfo.cnt Nothing Nothing False (Just ghInfo.expiryTime) currTime) expTime

resetDriverGoHomeRequest :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Merchant -> Id DP.Driver -> m ()
resetDriverGoHomeRequest merchantId driverId = do
  goHomeConfig <- CQGHC.findByMerchantId merchantId
  currTime <- getLocalCurrentTime =<< ((CQTC.findByMerchantId merchantId >>= fromMaybeM (InternalError "Transporter config for timezone not found")) <&> (.timeDiffFromUtc))
  let ghKey = makeGoHomeReqKey driverId
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  ghInfo <- getDriverGoHomeRequestInfo driverId merchantId
  Hedis.setExp ghKey (templateGoHomeData ghInfo.status ghInfo.cnt (Just $ addUTCTime (fromIntegral goHomeConfig.activeTime) currTime) ghInfo.driverGoHomeRequestId False (Just ghInfo.expiryTime) currTime) expTime

increaseDriverGoHomeRequestCount :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Merchant -> Id DP.Driver -> m ()
increaseDriverGoHomeRequestCount merchantId driverId = do
  currTime <- getLocalCurrentTime =<< ((CQTC.findByMerchantId merchantId >>= fromMaybeM (InternalError "Transporter config for timezone not found")) <&> (.timeDiffFromUtc))
  let ghKey = makeGoHomeReqKey driverId
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  ghInfo <- getDriverGoHomeRequestInfo driverId merchantId
  Hedis.setExp ghKey (templateGoHomeData ghInfo.status (ghInfo.cnt + 1) ghInfo.validTill ghInfo.driverGoHomeRequestId ghInfo.isOnRide (Just ghInfo.expiryTime) currTime) expTime

setDriverGoHomeIsOnRide :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id DP.Driver -> Id Merchant -> m ()
setDriverGoHomeIsOnRide driverId merchantId = do
  currTime <- getLocalCurrentTime =<< ((CQTC.findByMerchantId merchantId >>= fromMaybeM (InternalError "Transporter config for timezone not found")) <&> (.timeDiffFromUtc))
  let ghKey = makeGoHomeReqKey driverId
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  ghInfo <- getDriverGoHomeRequestInfo driverId merchantId
  Hedis.setExp ghKey (templateGoHomeData ghInfo.status ghInfo.cnt ghInfo.validTill ghInfo.driverGoHomeRequestId True (Just ghInfo.expiryTime) currTime) expTime
