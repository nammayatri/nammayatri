module Domain.Action.UI.FRFSBookingRatingInternal (postInternalFrfsBookingRating, getInternalFrfsBookingRating) where

import qualified API.Types.UI.FRFSBookingRatingInternal as API
import qualified Domain.Action.Beckn.Rating as BecknRating
import qualified Domain.Types.FRFSBookingRating as DFRFSBookingRating
import qualified Domain.Types.FRFSFleetStats as DFRFSFleetStats
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess (APISuccess (..))
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Storage.CachedQueries.Merchant as CQM
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.FRFSBookingRating as QFRFSBookingRating
import qualified Storage.Queries.FRFSFleetStats as QFRFSFleetStats
import qualified Storage.Queries.Person as QPerson
import Tools.Error

-- | Internal endpoint: rider-app (BAP) delivers a shuttle rating here after the passenger
-- deboards. The rider may rate the driver, the bus/fleet, or both (each optional). We resolve
-- the GIMS driver Person via the badge token and update the two aggregates independently:
--   * driver -> DriverStats (reusing the ride-hailing calculateAverageRating)
--   * bus/fleet -> FRFSFleetStats keyed by (gtfsId, fleetNumber)
-- The per-booking FRFSBookingRating row stores both values so a re-rate of either dimension
-- applies only that dimension's (new - old) delta.
postInternalFrfsBookingRating :: Maybe Text -> API.FRFSBookingRatingReq -> Environment.Flow APISuccess
postInternalFrfsBookingRating mbApiKey req = do
  let merchantId = Id req.merchantId
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantDoesNotExist req.merchantId)
  unless (Just merchant.internalApiKey == mbApiKey) $ throwError (InvalidRequest "Invalid internal api key for FRFS driver rating")
  validateRatingRange req.driverRating
  validateRatingRange req.fleetRating
  when (isJust req.fleetRating && (isNothing req.gtfsId || isNothing req.fleetNumber)) $
    throwError (InvalidRequest "gtfsId and fleetNumber are required when fleetRating is present")
  driver <- QPerson.findByOperatorBadgeTokenAndMerchantId (Just req.driverBadgeToken) merchantId >>= fromMaybeM (PersonNotFound req.driverBadgeToken)
  let driverId = driver.id
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = driver.merchantOperatingCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound driver.merchantOperatingCityId.getId)
  existing <- QFRFSBookingRating.findByBookingId req.bookingId

  -- driver aggregate: only when the rider rated the driver
  whenJust req.driverRating $ \newDriver ->
    Redis.withWaitAndLockRedis (mkDriverRatingLockKey driverId) 10 5000 $ do
      driverStats <- QDriverStats.findById driverId >>= fromMaybeM DriverInfoNotFound
      let (delta, shouldIncrement) = ratingDelta (existing >>= (.driverRatingValue)) newDriver
      void $ BecknRating.calculateAverageRating driverId merchant.minimumDriverRatesCount shouldIncrement delta driverStats.totalRatings driverStats.totalRatingScore transporterConfig

  -- bus/fleet aggregate: only when the rider rated the bus and we know which bus was boarded
  whenJust ((,,) <$> req.fleetRating <*> req.gtfsId <*> req.fleetNumber) $ \(newFleet, gtfsId, fleetNumber) ->
    Redis.withWaitAndLockRedis (mkFleetRatingLockKey gtfsId fleetNumber) 10 5000 $ do
      let (delta, shouldIncrement) = ratingDelta (existing >>= (.fleetRatingValue)) newFleet
      updateFleetRating merchantId driver.merchantOperatingCityId gtfsId fleetNumber delta shouldIncrement

  -- persist the per-booking record (merge: keep a prior value if this submission omits it)
  case existing of
    Nothing -> do
      row <- buildFRFSBookingRating merchantId driver req
      QFRFSBookingRating.create row
    Just old -> QFRFSBookingRating.updateRating (req.driverRating <|> old.driverRatingValue) (req.fleetRating <|> old.fleetRatingValue) (req.feedbackDetails <|> old.feedbackDetails) old.id
  pure Success

getInternalFrfsBookingRating ::
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Environment.Flow API.FRFSBookingRatingAggRes
getInternalFrfsBookingRating mbMerchantId mbDriverBadgeToken mbFleetNumber mbGtfsId mbApiKey = do
  merchantIdText <- mbMerchantId & fromMaybeM (InvalidRequest "merchantId is required")
  driverBadgeToken <- mbDriverBadgeToken & fromMaybeM (InvalidRequest "driverBadgeToken is required")
  let merchantId = Id merchantIdText
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantDoesNotExist merchantIdText)
  unless (Just merchant.internalApiKey == mbApiKey) $ throwError (InvalidRequest "Invalid internal api key for FRFS driver rating")
  driver <- QPerson.findByOperatorBadgeTokenAndMerchantId (Just driverBadgeToken) merchantId >>= fromMaybeM (PersonNotFound driverBadgeToken)
  mbDriverStats <- QDriverStats.findById driver.id
  -- The bus aggregate needs both halves of its key; without them there is simply no bus to report on.
  mbFleet <- case (,) <$> mbGtfsId <*> mbFleetNumber of
    Just (gtfsId, fleetNumber) -> QFRFSFleetStats.findByGtfsIdAndFleetNumber gtfsId fleetNumber
    Nothing -> pure Nothing
  pure
    API.FRFSBookingRatingAggRes
      { driverRating = mbDriverStats >>= (.rating),
        driverRatingCount = mbDriverStats >>= (.totalRatings),
        fleetRating = mbFleet >>= (.rating),
        fleetRatingCount = (.totalRatingCount) <$> mbFleet
      }

-- | Delta and count-increment for a running (sum, count) aggregate: a first rating adds its
-- full value and bumps the count; a re-rating adds only (new - old) and leaves the count.
ratingDelta :: Maybe Int -> Int -> (Int, Bool)
ratingDelta mbOld new = case mbOld of
  Nothing -> (new, True)
  Just old -> (new - old, False)

validateRatingRange :: Maybe Int -> Environment.Flow ()
validateRatingRange = maybe (pure ()) $ \v -> unless (v >= 1 && v <= 5) $ throwError (InvalidRequest "Rating value should be between 1 and 5")

mkDriverRatingLockKey :: Id DP.Person -> Text
mkDriverRatingLockKey driverId = "FRFS:DriverRating:DriverId-" <> driverId.getId

mkFleetRatingLockKey :: Text -> Text -> Text
mkFleetRatingLockKey gtfsId fleetNumber = "FRFS:FleetRating:" <> gtfsId <> ":" <> fleetNumber

buildFRFSBookingRating :: Id DM.Merchant -> DP.Person -> API.FRFSBookingRatingReq -> Environment.Flow DFRFSBookingRating.FRFSBookingRating
buildFRFSBookingRating merchantId driver req = do
  ratingId <- generateGUID
  now <- getCurrentTime
  pure
    DFRFSBookingRating.FRFSBookingRating
      { id = ratingId,
        bookingId = req.bookingId,
        driverId = driver.id,
        operatorBadgeToken = req.driverBadgeToken,
        fleetNumber = req.fleetNumber,
        gtfsId = req.gtfsId,
        driverRatingValue = req.driverRating,
        fleetRatingValue = req.fleetRating,
        feedbackDetails = req.feedbackDetails,
        merchantId = Just merchantId,
        merchantOperatingCityId = Just driver.merchantOperatingCityId,
        createdAt = now,
        updatedAt = now
      }

updateFleetRating :: Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Text -> Text -> Int -> Bool -> Environment.Flow ()
updateFleetRating merchantId merchantOperatingCityId gtfsId fleetNumber delta shouldIncrement = do
  mbFleet <- QFRFSFleetStats.findByGtfsIdAndFleetNumber gtfsId fleetNumber
  case mbFleet of
    Just fleet -> do
      let newScore = fleet.totalRatingScore + delta
          newCount = fleet.totalRatingCount + (if shouldIncrement then 1 else 0)
      QFRFSFleetStats.updateRatingAgg newScore newCount (mkFleetRating newScore newCount) fleet.id
    Nothing -> do
      fleetRatingId <- generateGUID
      now <- getCurrentTime
      let newScore = delta
          newCount = if shouldIncrement then 1 else 0
      QFRFSFleetStats.create
        DFRFSFleetStats.FRFSFleetStats
          { id = fleetRatingId,
            gtfsId = gtfsId,
            fleetNumber = fleetNumber,
            totalRatingScore = newScore,
            totalRatingCount = newCount,
            rating = mkFleetRating newScore newCount,
            merchantId = Just merchantId,
            merchantOperatingCityId = Just merchantOperatingCityId,
            createdAt = now,
            updatedAt = now
          }

mkFleetRating :: Int -> Int -> Maybe Centesimal
mkFleetRating score count
  | count > 0 = Just (fromIntegral score / fromIntegral count)
  | otherwise = Just 0
