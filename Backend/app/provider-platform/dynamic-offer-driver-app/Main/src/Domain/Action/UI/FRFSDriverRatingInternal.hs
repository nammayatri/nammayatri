{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.FRFSDriverRatingInternal (postInternalFrfsDriverRating) where

import qualified API.Types.UI.FRFSDriverRatingInternal as API
import qualified Domain.Action.Beckn.Rating as BecknRating
import qualified Domain.Types.FRFSDriverRating as DFRFSDriverRating
import qualified Domain.Types.FRFSFleetRating as DFRFSFleetRating
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess (APISuccess (..))
import Kernel.Types.Common (Centesimal)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.FRFSDriverRating as QFRFSDriverRating
import qualified Storage.Queries.FRFSFleetRating as QFRFSFleetRating
import qualified Storage.Queries.Person as QPerson
import Tools.Error

-- | Internal endpoint: rider-app (BAP) delivers a shuttle rating here after the passenger
-- deboards. The rider may rate the driver, the bus/fleet, or both (each optional). We resolve
-- the GIMS driver Person via the badge token and update the two aggregates independently:
--   * driver -> DriverStats (reusing the ride-hailing calculateAverageRating)
--   * bus/fleet -> FRFSFleetRating keyed by (gtfsId, fleetNumber)
-- The per-booking FRFSDriverRating row stores both values so a re-rate of either dimension
-- applies only that dimension's (new - old) delta.
postInternalFrfsDriverRating :: Maybe Text -> API.FRFSDriverRatingReq -> Environment.Flow APISuccess
postInternalFrfsDriverRating mbApiKey req = do
  let merchantId = Id req.merchantId
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantDoesNotExist req.merchantId)
  unless (Just merchant.internalApiKey == mbApiKey) $ throwError (InvalidRequest "Invalid internal api key for FRFS driver rating")
  validateRatingRange req.driverRating
  validateRatingRange req.fleetRating
  driver <- QPerson.findByOperatorBadgeTokenAndMerchantId (Just req.driverBadgeToken) merchantId >>= fromMaybeM (PersonNotFound req.driverBadgeToken)
  let driverId = driver.id
  transporterConfig <- SCTC.findByMerchantOpCityId driver.merchantOperatingCityId Nothing >>= fromMaybeM (TransporterConfigNotFound driver.merchantOperatingCityId.getId)
  existing <- QFRFSDriverRating.findByBookingId req.bookingId

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
      row <- buildFRFSDriverRating merchantId driver req
      QFRFSDriverRating.create row
    Just old -> QFRFSDriverRating.updateRating (req.driverRating <|> old.driverRatingValue) (req.fleetRating <|> old.fleetRatingValue) req.feedbackDetails old.id
  pure Success

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

buildFRFSDriverRating :: Id DM.Merchant -> DP.Person -> API.FRFSDriverRatingReq -> Environment.Flow DFRFSDriverRating.FRFSDriverRating
buildFRFSDriverRating merchantId driver req = do
  ratingId <- generateGUID
  now <- getCurrentTime
  pure
    DFRFSDriverRating.FRFSDriverRating
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
  mbFleet <- QFRFSFleetRating.findByGtfsIdAndFleetNumber gtfsId fleetNumber
  case mbFleet of
    Just fleet -> do
      let newScore = fleet.totalRatingScore + delta
          newCount = fleet.totalRatingCount + (if shouldIncrement then 1 else 0)
      QFRFSFleetRating.updateRatingAgg newScore newCount (mkFleetRating newScore newCount) fleet.id
    Nothing -> do
      fleetRatingId <- generateGUID
      now <- getCurrentTime
      let newScore = delta
          newCount = if shouldIncrement then 1 else 0
      QFRFSFleetRating.create
        DFRFSFleetRating.FRFSFleetRating
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
