{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Storage.CachedQueries.Driver.GoHomeRequest where

import Control.Monad
import Data.Text (pack)
import Domain.Types.Driver.GoHomeFeature.DriverGoHomeRequest as DDGR
import Domain.Types.Driver.GoHomeFeature.DriverHomeLocation as DDHL
import qualified Domain.Types.Person as DP
import qualified EulerHS.Language as L
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.App (MonadFlow)
import Kernel.Types.Common (MonadTime (getCurrentTime), generateGUID)
import Kernel.Types.Id (Id)
import Kernel.Types.SlidingWindowCounters (PeriodType (Days))
import Kernel.Utils.Common (addUTCTime, fromMaybeM, getLocalCurrentTime, secondsToNominalDiffTime)
import Kernel.Utils.SlidingWindowCounters (incrementPeriod)
import Storage.CachedQueries.CacheConfig (CacheFlow)
import Storage.Queries.Driver.GoHomeFeature.DriverGoHomeRequest as BeamDHR
import Storage.Queries.Ride as Ride
import Tools.Error (GenericError (..))

makeGoHomeReqKey :: Id DP.Driver -> Text
makeGoHomeReqKey = pack . ("CachedQueries:GoHomeRequest-driverId:" <>) . show

getDriverGoHomeRequestInfo :: (CacheFlow m r, MonadFlow m) => Id DP.Driver -> m CachedGoHomeRequest
getDriverGoHomeRequestInfo driverId = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let ghkey = makeGoHomeReqKey driverId
  currTime <- getLocalCurrentTime 19800
  Hedis.safeGet ghkey >>= \case
    Just ghrData -> do
      if fromMaybe False $ liftM2 (||) (ghrData.validTill <&> (> currTime)) (pure ghrData.isOnRide)
        then return ghrData
        else checkInvalidReqData ghrData currTime ghkey -- OnRide Condition
    Nothing -> do
      Hedis.setExp ghkey (templateGoHomeData Nothing 2 Nothing Nothing False Nothing currTime) expTime
      return $ templateGoHomeData Nothing 2 Nothing Nothing False Nothing currTime

checkInvalidReqData :: (CacheFlow m r, L.MonadFlow m) => CachedGoHomeRequest -> UTCTime -> Text -> m CachedGoHomeRequest
checkInvalidReqData ghrData currTime ghkey = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  if ghrData.expiryTime > currTime
    then case ghrData.validTill of
      Just _ -> do
        ghrId <- fromMaybeM (InternalError "GoHomeRequestId not present but valid Till set") ghrData.driverGoHomeRequestId
        succRide <- Ride.findSuccRideByGoHomeRequestId ghrId
        if isJust succRide
          then do
            Hedis.setExp ghkey (templateGoHomeData Nothing (ghrData.cnt - 1) Nothing Nothing False (Just ghrData.expiryTime) currTime) expTime
            return (templateGoHomeData Nothing (ghrData.cnt - 1) Nothing Nothing False (Just ghrData.expiryTime) currTime)
          else do
            Hedis.setExp ghkey (templateGoHomeData Nothing ghrData.cnt Nothing Nothing False (Just ghrData.expiryTime) currTime) expTime
            return (templateGoHomeData Nothing ghrData.cnt Nothing Nothing False (Just ghrData.expiryTime) currTime)
      Nothing -> do
        return ghrData
    else do
      Hedis.setExp ghkey (templateGoHomeData Nothing 2 Nothing Nothing False Nothing currTime) expTime
      return $ templateGoHomeData Nothing 2 Nothing Nothing False Nothing currTime

templateGoHomeData :: Maybe DriverGoHomeRequestStatus -> Int -> Maybe UTCTime -> Maybe (Id DriverGoHomeRequest) -> Bool -> Maybe UTCTime -> UTCTime -> CachedGoHomeRequest
templateGoHomeData stat count vTill ghrId isOnRde expiryTime currTime =
  CachedGoHomeRequest
    { status = stat,
      cnt = count,
      validTill = vTill,
      driverGoHomeRequestId = ghrId,
      isOnRide = isOnRde,
      expiryTime = fromMaybe (addUTCTime 0 (incrementPeriod Days currTime)) expiryTime
    }

activateDriverGoHomeRequest :: (MonadFlow m, CacheFlow m r) => Id DP.Driver -> DDHL.DriverHomeLocation -> m ()
activateDriverGoHomeRequest driverId driverHomeLoc = do
  let ghKey = makeGoHomeReqKey driverId
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  currTime <- getLocalCurrentTime 19800
  guId <- generateGUID
  _ <- BeamDHR.create =<< buildDriverGoHomeRequest guId driverHomeLoc
  ghInfo <- getDriverGoHomeRequestInfo driverId
  let vTill = addUTCTime (secondsToNominalDiffTime 1800) currTime
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

deactivateDriverGoHomeRequest :: (MonadFlow m, CacheFlow m r) => Id DP.Driver -> Maybe DDGR.DriverGoHomeRequestStatus -> m ()
deactivateDriverGoHomeRequest driverId mbStatus = do
  currTime <- getLocalCurrentTime 19800
  let ghKey = makeGoHomeReqKey driverId
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  ghInfo <- getDriverGoHomeRequestInfo driverId
  driverGoHomeReqId <- fromMaybeM (InternalError "Could not Find DriverGoHomeRequestId") ghInfo.driverGoHomeRequestId
  case mbStatus of
    Just SUCCESS -> do
      BeamDHR.finishSuccessfully driverGoHomeReqId
      Hedis.setExp ghKey (templateGoHomeData Nothing (ghInfo.cnt - 1) Nothing Nothing False (Just ghInfo.expiryTime) currTime) expTime
    Just FAILED -> do
      BeamDHR.finishWithFailure driverGoHomeReqId
      Hedis.setExp ghKey (templateGoHomeData Nothing ghInfo.cnt Nothing Nothing False (Just ghInfo.expiryTime) currTime) expTime
    Nothing -> do
      succRide <- Ride.findSuccRideByGoHomeRequestId driverGoHomeReqId
      if isJust succRide
        then do
          BeamDHR.finishSuccessfully driverGoHomeReqId
          Hedis.setExp ghKey (templateGoHomeData Nothing (ghInfo.cnt - 1) Nothing Nothing False (Just ghInfo.expiryTime) currTime) expTime
        else do
          BeamDHR.finishWithFailure driverGoHomeReqId
          Hedis.setExp ghKey (templateGoHomeData Nothing ghInfo.cnt Nothing Nothing False (Just ghInfo.expiryTime) currTime) expTime

resetDriverGoHomeRequest :: (MonadFlow m, CacheFlow m r) => Id DP.Driver -> m ()
resetDriverGoHomeRequest driverId = do
  currTime <- getLocalCurrentTime 19800
  let ghKey = makeGoHomeReqKey driverId
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  ghInfo <- getDriverGoHomeRequestInfo driverId
  Hedis.setExp ghKey (templateGoHomeData ghInfo.status ghInfo.cnt (Just $ addUTCTime 1800 currTime) ghInfo.driverGoHomeRequestId False (Just ghInfo.expiryTime) currTime) expTime
