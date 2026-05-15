module SharedLogic.PassExpiryReminder where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Time as Time
import Domain.Types.EmptyDynamicParam (EmptyDynamicParam (..))
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.PurchasedPass as DPurchasedPass
import EulerHS.Prelude hiding (pass)
import qualified Kernel.External.Notification as Notification
import Kernel.External.Types (ServiceFlow)
import Kernel.Types.Id (Id, getId)
import Kernel.Utils.Common
import qualified Storage.ConfigPilot.Config.RiderConfig as SCRC
import Storage.ConfigPilot.Interface.Types (getConfig)
import qualified Storage.Queries.PersonExtra as QPerson
import qualified Storage.Queries.PurchasedPass as QPurchasedPass
import Tools.Error
import qualified Tools.Notifications as TNotifications

defaultDaysToExpire :: Int
defaultDaysToExpire = 3

defaultBatchSize :: Int
defaultBatchSize = 100

newtype PassExpiryReminderEntityData = PassExpiryReminderEntityData
  { notificationKey :: Text
  }
  deriving (Generic, Show, ToJSON)

-- Returns Just (offset + processed) if the batch was full (more rows likely), or Nothing when drained.
sendPassExpiryReminderBatch ::
  (ServiceFlow m r, EsqDBFlow m r, CacheFlow m r) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Maybe Int ->
  m (Maybe Int)
sendPassExpiryReminderBatch merchantId merchantOperatingCityId mbCursor = do
  riderConfig <-
    getConfig (SCRC.RiderDimensions {merchantOperatingCityId = merchantOperatingCityId.getId})
      >>= fromMaybeM (RiderConfigDoesNotExist merchantOperatingCityId.getId)
  let daysToExpire = fromMaybe defaultDaysToExpire riderConfig.passExpiryReminderDays
      remindDaily = fromMaybe False riderConfig.remindEverydayUntilPassExpiry
      batchSize = max 1 $ fromMaybe defaultBatchSize riderConfig.passExpiryReminderBatchSize
  if daysToExpire <= 0
    then pure Nothing
    else do
      now <- getCurrentTime
      let localNow = Time.addUTCTime (secondsToNominalDiffTime riderConfig.timeDiffFromUtc) now
          todayLocal = Time.utctDay localNow
          lastDay = Time.addDays (fromIntegral daysToExpire) todayLocal
          fromDay = if remindDaily then Time.addDays 1 todayLocal else lastDay
      passes <-
        QPurchasedPass.findActiveExpiringInRangeByCursor merchantId merchantOperatingCityId fromDay lastDay mbCursor batchSize
      let personIdToEndDate = Map.fromListWith min [(p.personId, p.endDate) | p <- passes]
          activePersonIds = Map.keys personIdToEndDate
      unless (null activePersonIds) $ do
        prebookedPassesForActivePersonIds <- QPurchasedPass.findAllByPersonIdsAndStatusBatched activePersonIds DPurchasedPass.PreBooked
        let prebookedPersonIds = Set.fromList $ map (.personId) prebookedPassesForActivePersonIds
            finalPersonIds = filter (`Set.notMember` prebookedPersonIds) activePersonIds
        persons <- QPerson.findAllByPersonIds (map getId finalPersonIds)
        forM_ persons $ \person -> do
          let mbEndDate = Map.lookup person.id personIdToEndDate
              daysRemaining = maybe daysToExpire (fromIntegral . flip Time.diffDays todayLocal) mbEndDate
              entity = Notification.Entity Notification.Product person.id.getId (PassExpiryReminderEntityData {notificationKey = "PASS_EXPIRY_REMINDER"})
              templateParams = [("days", show daysRemaining)]
          TNotifications.dynamicNotifyPerson person (TNotifications.createNotificationReq "PASS_EXPIRY_REMINDER" identity) EmptyDynamicParam entity Nothing templateParams Nothing Nothing
      let processed = length passes
      pure $
        if processed < batchSize
          then Nothing
          else Just (fromMaybe 0 mbCursor + processed)
