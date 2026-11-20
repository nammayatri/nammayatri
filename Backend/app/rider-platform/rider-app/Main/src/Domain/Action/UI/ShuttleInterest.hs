module Domain.Action.UI.ShuttleInterest
  ( postShuttleInterest,
  )
where

import qualified API.Types.UI.ShuttleInterest as API
import Data.List (nub)
import qualified Data.Text as T
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.ShuttleInterest as DShuttleInterest
import qualified Environment
import Kernel.External.Encryption (decrypt)
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.ShuttleInterest as QShuttleInterest
import Tools.Error

allowedTimeFlexibilityMinutes :: [Int]
allowedTimeFlexibilityMinutes = [15, 30]

-- | Submit the corporate shuttle interest form (page 2). Page 1 is the existing business-email OTP flow
-- (/auth/business-email/send-verification and /verify); the form is accepted only after it. A second submission replaces
-- the first (one row per person).
postShuttleInterest :: (Maybe (Id DP.Person), Id DM.Merchant) -> API.ShuttleInterestReq -> Environment.Flow APISuccess
postShuttleInterest (mbPersonId, _) req = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  unless (person.businessProfileVerified == Just True) $
    throwError $ InvalidRequest "Please verify your work email to continue"
  businessEmail <- mapM decrypt person.businessEmail
  companyDomain <- (businessEmail >>= emailDomain) & fromMaybeM (InvalidRequest "Please verify your work email to continue")
  validateRequest req
  now <- getCurrentTime
  mbExisting <- QShuttleInterest.findByPrimaryKey personId
  let interest =
        DShuttleInterest.ShuttleInterest
          { DShuttleInterest.personId = personId,
            DShuttleInterest.companyDomain = companyDomain,
            DShuttleInterest.pickupLat = req.pickupLat,
            DShuttleInterest.pickupLon = req.pickupLon,
            DShuttleInterest.pickupAddress = T.strip req.pickupAddress,
            DShuttleInterest.dropLat = req.dropLat,
            DShuttleInterest.dropLon = req.dropLon,
            DShuttleInterest.dropAddress = T.strip req.dropAddress,
            DShuttleInterest.tripType = req.tripType,
            -- a leg the employee did not ask for keeps no time
            DShuttleInterest.pickupTime = if needsPickupTime req.tripType then req.pickupTime else Nothing,
            DShuttleInterest.dropTime = if needsDropTime req.tripType then req.dropTime else Nothing,
            DShuttleInterest.shiftStartTime = req.shiftStartTime,
            DShuttleInterest.shiftEndTime = req.shiftEndTime,
            DShuttleInterest.maxWalkingDistanceMeters = req.maxWalkingDistanceMeters,
            DShuttleInterest.daysOfWeek = nub req.daysOfWeek,
            DShuttleInterest.timeFlexibilityMinutes = req.timeFlexibilityMinutes,
            DShuttleInterest.acPreference = req.acPreference,
            DShuttleInterest.vehicleSizePreference = req.vehicleSizePreference,
            DShuttleInterest.merchantId = person.merchantId,
            DShuttleInterest.merchantOperatingCityId = person.merchantOperatingCityId,
            DShuttleInterest.createdAt = maybe now (.createdAt) mbExisting,
            DShuttleInterest.updatedAt = now
          }
  case mbExisting of
    Just _ -> QShuttleInterest.updateByPrimaryKey interest
    Nothing -> QShuttleInterest.create interest
  pure Success

-- | Lower-cased domain of an email, same parsing as the business-email domain in Domain.Action.UI.Search.
emailDomain :: Text -> Maybe Text
emailDomain email =
  let domain = T.toLower . T.drop 1 . T.strip . snd $ T.breakOn "@" email
   in if T.isInfixOf "@" email && not (T.null domain) then Just domain else Nothing

needsPickupTime :: DShuttleInterest.ShuttleTripType -> Bool
needsPickupTime tripType = tripType /= DShuttleInterest.TO_HOME

needsDropTime :: DShuttleInterest.ShuttleTripType -> Bool
needsDropTime tripType = tripType /= DShuttleInterest.TO_OFFICE

validateRequest :: API.ShuttleInterestReq -> Environment.Flow ()
validateRequest req = do
  unless (validLatLon req.pickupLat req.pickupLon) $ throwError $ InvalidRequest "Invalid pickup location"
  unless (validLatLon req.dropLat req.dropLon) $ throwError $ InvalidRequest "Invalid drop location"
  when (T.null (T.strip req.pickupAddress)) $ throwError $ InvalidRequest "Pickup address is required"
  when (T.null (T.strip req.dropAddress)) $ throwError $ InvalidRequest "Drop address is required"
  when (needsPickupTime req.tripType && isNothing req.pickupTime) $
    throwError $ InvalidRequest "Pickup time is required for this trip type"
  when (needsDropTime req.tripType && isNothing req.dropTime) $
    throwError $ InvalidRequest "Drop time is required for this trip type"
  when (null req.daysOfWeek) $ throwError $ InvalidRequest "Select at least one day of the week"
  unless (req.maxWalkingDistanceMeters > 0) $ throwError $ InvalidRequest "Walking distance must be greater than 0"
  unless (req.timeFlexibilityMinutes `elem` allowedTimeFlexibilityMinutes) $
    throwError $ InvalidRequest "Time flexibility must be 15 or 30 minutes"
  where
    validLatLon lat lon = lat >= -90 && lat <= 90 && lon >= -180 && lon <= 180
