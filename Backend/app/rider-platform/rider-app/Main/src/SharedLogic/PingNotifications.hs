module SharedLogic.PingNotifications where

import Data.Time hiding (getCurrentTime)
import qualified Domain.Types.Booking as DRB
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.Queries.Person as QPerson
import Tools.Error
import qualified Tools.Notifications as Notify

pushFeedBackRequestNotification ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    EncFlow m r,
    EsqDBReplicaFlow m r
  ) =>
  DRB.Booking ->
  m ()
pushFeedBackRequestNotification booking = do
  let feedBackRequestRedisKey = "feedback-request:" <> show booking.id
  feedbackReq :: Maybe Bool <- Redis.safeGet feedBackRequestRedisKey
  void $ case feedbackReq of
    Just _ -> pure unit
    Nothing -> do
      person <- QPerson.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
      riderConfig <- QRC.findByMerchantOperatingCityId person.merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist person.merchantOperatingCityId.getId)
      let bookingStartTime = booking.startTime
          bookingEstimatedDuration = fromMaybe (Seconds 0) booking.estimatedDuration
          multipier = fromIntegral (fromMaybe 50 riderConfig.feedBackFCMDelayMultipler) :: NominalDiffTime
          bufferTime = (multipier * 0.01) * (fromIntegral $ getSeconds bookingEstimatedDuration)
          bookingBufferTime = addUTCTime bufferTime bookingStartTime
      now <- getCurrentTime
      if (now >= bookingBufferTime)
        then do
          Redis.setExp feedBackRequestRedisKey booking.id (getSeconds bookingEstimatedDuration)
          void $ Notify.sendFeedbackRequest booking
          pure unit
        else pure unit
