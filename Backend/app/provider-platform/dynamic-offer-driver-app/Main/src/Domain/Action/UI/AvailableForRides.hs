-- | Driver-facing switch for the "available for rides" boost.
--
-- Writes the @AvailableForRides@ tag onto 'person.driverTag' with a configurable
-- minute-granularity expiry, and opens a fresh search-request budget for it. The tag's
-- lifecycle (and how it is spent and removed) lives in
-- 'SharedLogic.DriverPool.AvailableForRides'.
module Domain.Action.UI.AvailableForRides (postDriverAvailableForRidesActivate) where

import qualified API.Types.UI.AvailableForRides as APIT
import qualified Data.Time as DT
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.Yudhishthira.Tools.Utils as Yudhishthira
import qualified SharedLogic.DriverIdleTime as DriverIdleTime
import qualified SharedLogic.DriverPool.AvailableForRides as AvailableForRides
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.Person as QPerson
import Tools.Error

postDriverAvailableForRidesActivate ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.Flow APIT.AvailableForRidesRes
  )
postDriverAvailableForRidesActivate (mbPersonId, _, merchantOpCityId) = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  transporterConfig <-
    getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing
      >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  -- All three knobs must be set for the feature to be live in this city: a boost with no
  -- expiry, no daily cap or no request budget is not a boost we are willing to hand out.
  (validity, dailyLimit, maxRequests) <- AvailableForRides.enabledConfig transporterConfig & fromMaybeM AvailableForRidesNotEnabled

  case mfilter ((> 0) . (.getMinutes)) transporterConfig.availableForRidesMinIdleMinutes of
    Nothing -> pure ()
    Just minIdle -> do
      mbIdleSeconds <- DriverIdleTime.getIdleTimeSeconds personId
      when (maybe False (< fromIntegral (minIdle.getMinutes * 60)) mbIdleSeconds) $
        throwError (AvailableForRidesNotIdleEnough minIdle.getMinutes)

  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  now <- getCurrentTime
  localDay <- DT.utctDay <$> getLocalCurrentTime transporterConfig.timeDiffFromUtc
  AvailableForRides.settleExpiredBoost personId localDay
  -- Claim the slot before doing anything else: the INCR is the atomic gate, so two
  -- concurrent taps can't both slip through on a stale read. A rejected attempt is handed
  -- straight back, so the counter only ever counts boosts that were actually granted.
  activationsUsedToday <- AvailableForRides.claimActivation personId localDay dailyLimit >>= fromMaybeM (AvailableForRidesDailyLimitExceeded dailyLimit)

  -- Re-activating on top of a live boost is allowed; it replaces the tag (so the expiry
  -- restarts) and resets the request budget and rejection streak.
  let tag = AvailableForRides.mkAvailableForRidesTag validity now
      validTill = addUTCTime (fromIntegral $ validity.getMinutes * 60) now
  QPerson.updateDriverTag (Just $ Yudhishthira.replaceTagNameValue person.driverTag tag) personId
  AvailableForRides.startBoost personId localDay validity validTill
  pure
    APIT.AvailableForRidesRes
      { validTill,
        validityMinutes = validity,
        activationsUsedToday,
        activationsAllowedPerDay = dailyLimit,
        requestsAllowed = maxRequests
      }
