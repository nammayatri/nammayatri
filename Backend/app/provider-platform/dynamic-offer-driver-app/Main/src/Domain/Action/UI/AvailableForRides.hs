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
  (validity, dailyLimit, maxRequests) <-
    (,,) <$> transporterConfig.availableForRidesTagValidityMinutes
      <*> transporterConfig.availableForRidesDailyLimit
      <*> transporterConfig.availableForRidesMaxSearchRequests
      & fromMaybeM AvailableForRidesNotEnabled
  unless (validity.getMinutes > 0 && dailyLimit > 0 && maxRequests > 0) $ throwError AvailableForRidesNotEnabled

  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  now <- getCurrentTime
  localDay <- DT.utctDay <$> getLocalCurrentTime transporterConfig.timeDiffFromUtc
  -- Claim the slot before doing anything else: the INCR is the atomic gate, so two
  -- concurrent taps can't both slip through on a stale read. A rejected attempt leaves the
  -- counter above the limit, which simply keeps the driver rejected for the rest of the day.
  activationsUsedToday <- AvailableForRides.recordActivation personId localDay
  when (activationsUsedToday > dailyLimit) $ throwError (AvailableForRidesDailyLimitExceeded dailyLimit)

  -- Re-activating on top of a live boost is allowed; it replaces the tag (so the expiry
  -- restarts) and, below, resets the request budget.
  let tag = AvailableForRides.mkAvailableForRidesTag validity now
  QPerson.updateDriverTag (Just $ Yudhishthira.replaceTagNameValue person.driverTag tag) personId
  AvailableForRides.startRequestBudget personId validity
  pure
    APIT.AvailableForRidesRes
      { validTill = addUTCTime (fromIntegral $ validity.getMinutes * 60) now,
        validityMinutes = validity,
        activationsUsedToday,
        activationsAllowedPerDay = dailyLimit,
        requestsAllowed = maxRequests
      }
