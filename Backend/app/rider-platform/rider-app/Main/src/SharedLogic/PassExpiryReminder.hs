module SharedLogic.PassExpiryReminder where

import qualified Data.List as List
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

sendPassExpiryReminderPns ::
  (ServiceFlow m r, EsqDBFlow m r, CacheFlow m r) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  m ()
sendPassExpiryReminderPns merchantId merchantOperatingCityId = do
  riderConfig <-
    getConfig (SCRC.RiderDimensions {merchantOperatingCityId = merchantOperatingCityId.getId})
      >>= fromMaybeM (RiderConfigDoesNotExist merchantOperatingCityId.getId)
  let daysToExpire = fromMaybe 3 (riderConfig.passExpiryReminderDays)
  unless (daysToExpire < 0) $ do
    now <- getCurrentTime
    let localNow = Time.addUTCTime (secondsToNominalDiffTime riderConfig.timeDiffFromUtc) now
        todayLocal = Time.utctDay localNow
        targetEndDate = Time.addDays (fromIntegral daysToExpire) todayLocal
    purchasedPasses <- QPurchasedPass.findAllByEndDateMerchantOperatingCityIdAndMerchantId targetEndDate merchantOperatingCityId merchantId
    let activePurchasedPasses = filter (\pass -> pass.status == DPurchasedPass.Active) purchasedPasses -- This is technically not needed, but I added incase purchase is pending. Can remove if not required
    let activePersonIds = List.nub $ map (.personId) activePurchasedPasses -- All personIds with active passes
    unless (null activePersonIds) $ do
      prebookedPassesForActivePersonIds <- QPurchasedPass.findAllByPersonIdsAndStatusBatched activePersonIds DPurchasedPass.PreBooked
      let prebookedPersonIds = Set.fromList $ List.nub $ map (.personId) prebookedPassesForActivePersonIds
          finalPersonIds = filter (`Set.notMember` prebookedPersonIds) activePersonIds
      persons <- QPerson.findAllByPersonIds (map getId finalPersonIds)
      forM_ persons $ \person -> do
        let entity = Notification.Entity Notification.Product person.id.getId ()
            templateParams = [("days", show daysToExpire)]
        TNotifications.dynamicNotifyPerson person (TNotifications.createNotificationReq "PASS_EXPIRY_REMINDER" identity) EmptyDynamicParam entity Nothing templateParams Nothing Nothing
