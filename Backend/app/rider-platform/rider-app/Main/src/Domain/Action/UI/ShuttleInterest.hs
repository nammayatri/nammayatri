module Domain.Action.UI.ShuttleInterest
  ( postShuttleInterest,
    getShuttleEligibility,
  )
where

import qualified API.Types.UI.ShuttleInterest as API
import Data.List (nub)
import qualified Data.Text as T
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.PassOrganization as DPassOrganization
import qualified Domain.Types.Person as DP
import qualified Domain.Types.ShuttleInterest as DShuttleInterest
import qualified Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.CorporateMembership as SCorporate
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.ShuttleInterest as QShuttleInterest
import Tools.Error
import qualified Tools.Maps as Maps

-- | Submit the corporate shuttle interest form (page 2). Page 1 is the existing business-email OTP flow
-- (/auth/business-email/send-verification and /verify); the form is accepted only after it, and only when the email's
-- domain belongs to a company (SharedLogic.CorporateMembership). A second submission replaces the first (one row per person).
postShuttleInterest :: (Maybe (Id DP.Person), Id DM.Merchant) -> API.ShuttleInterestReq -> Environment.Flow APISuccess
postShuttleInterest (mbPersonId, _) req = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  (company, companyDomain) <- SCorporate.requireCorporateOrg person
  validateRequest req
  -- the location the employee chose (their GPS position or the address they picked) is kept exactly; only a missing
  -- address is filled in, from those same coordinates
  pickupAddress <- resolveAddress person "pickup" req.pickupLat req.pickupLon req.pickupAddress
  dropAddress <- resolveAddress person "drop" req.dropLat req.dropLon req.dropAddress
  now <- getCurrentTime
  mbExisting <- QShuttleInterest.findByPrimaryKey personId
  let interest =
        DShuttleInterest.ShuttleInterest
          { DShuttleInterest.personId = personId,
            DShuttleInterest.passOrganizationId = company.id,
            DShuttleInterest.companyDomain = companyDomain,
            DShuttleInterest.pickupLat = req.pickupLat,
            DShuttleInterest.pickupLon = req.pickupLon,
            DShuttleInterest.pickupAddress = pickupAddress,
            DShuttleInterest.pickupLocationSource = req.pickupLocationSource,
            DShuttleInterest.pickupAccuracyMeters = req.pickupAccuracyMeters,
            DShuttleInterest.dropLat = req.dropLat,
            DShuttleInterest.dropLon = req.dropLon,
            DShuttleInterest.dropAddress = dropAddress,
            DShuttleInterest.dropLocationSource = req.dropLocationSource,
            DShuttleInterest.dropAccuracyMeters = req.dropAccuracyMeters,
            DShuttleInterest.tripType = req.tripType,
            -- a leg the employee did not ask for keeps no time
            DShuttleInterest.pickupTime = if needsPickupTime req.tripType then req.pickupTime else Nothing,
            DShuttleInterest.dropTime = if needsDropTime req.tripType then req.dropTime else Nothing,
            DShuttleInterest.daysOfWeek = nub req.daysOfWeek,
            DShuttleInterest.merchantId = person.merchantId,
            DShuttleInterest.merchantOperatingCityId = person.merchantOperatingCityId,
            DShuttleInterest.createdAt = maybe now (.createdAt) mbExisting,
            DShuttleInterest.updatedAt = now
          }
  case mbExisting of
    Just _ -> QShuttleInterest.updateByPrimaryKey interest
    Nothing -> QShuttleInterest.create interest
  pure Success

-- | Between the work-email OTP and the form: whether the caller can fill it, and for which company. The company's
-- address prefills the form's drop.
getShuttleEligibility :: (Maybe (Id DP.Person), Id DM.Merchant) -> Environment.Flow API.ShuttleEligibilityResp
getShuttleEligibility (mbPersonId, _) = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  mbCompany <- fmap fst <$> SCorporate.findCorporateOrg person
  hasSubmittedInterest <- isJust <$> QShuttleInterest.findByPrimaryKey personId
  pure $ mkEligibility person mbCompany hasSubmittedInterest

mkEligibility :: DP.Person -> Maybe DPassOrganization.PassOrganization -> Bool -> API.ShuttleEligibilityResp
mkEligibility person mbCompany hasSubmittedInterest =
  API.ShuttleEligibilityResp
    { API.workEmailVerified = person.businessProfileVerified == Just True,
      API.eligible = isJust mbCompany,
      API.companyName = (.name) <$> mbCompany,
      API.officeAddress = mbCompany >>= (.address),
      API.officeLat = mbCompany >>= (.officeLat),
      API.officeLon = mbCompany >>= (.officeLon),
      API.hasSubmittedInterest = hasSubmittedInterest
    }

needsPickupTime :: DShuttleInterest.ShuttleTripType -> Bool
needsPickupTime tripType = tripType /= DShuttleInterest.TO_HOME

needsDropTime :: DShuttleInterest.ShuttleTripType -> Bool
needsDropTime tripType = tripType /= DShuttleInterest.TO_OFFICE

validateRequest :: API.ShuttleInterestReq -> Environment.Flow ()
validateRequest req = do
  unless (validLatLon req.pickupLat req.pickupLon) $ throwError $ InvalidRequest "Invalid pickup location"
  unless (validLatLon req.dropLat req.dropLon) $ throwError $ InvalidRequest "Invalid drop location"
  checkGps "pickup" req.pickupLocationSource req.pickupAccuracyMeters
  checkGps "drop" req.dropLocationSource req.dropAccuracyMeters
  when (needsPickupTime req.tripType && isNothing req.pickupTime) $
    throwError $
      InvalidRequest "Pickup time is required for this trip type"
  when (needsDropTime req.tripType && isNothing req.dropTime) $
    throwError $
      InvalidRequest "Drop time is required for this trip type"
  when (null req.daysOfWeek) $ throwError $ InvalidRequest "Select at least one day of the week"
  where
    validLatLon lat lon = lat >= -90 && lat <= 90 && lon >= -180 && lon <= 180 && not (lat == 0 && lon == 0)
    -- a GPS location must say how accurate it is, and be accurate enough to plan a pickup point 500 m away from; else
    -- the employee is asked to search for their address instead of the form keeping an approximate point
    checkGps leg source accuracy = when (source == Just DShuttleInterest.GPS) $ case accuracy of
      Nothing -> throwError $ InvalidRequest ("The " <> leg <> " GPS location has no accuracy; search for the address instead")
      Just meters
        | meters <= 0 || meters > maxGpsAccuracyMeters ->
          throwError $ InvalidRequest ("The " <> leg <> " GPS location is only accurate to " <> show (round meters :: Int) <> " m; search for the address instead")
      Just _ -> pure ()

-- | Worse than this, a GPS fix could be on the wrong street.
maxGpsAccuracyMeters :: Double
maxGpsAccuracyMeters = 50

-- | The address given, or else the formatted address of those exact coordinates (the coordinates are never changed).
resolveAddress :: DP.Person -> Text -> Double -> Double -> Maybe Text -> Environment.Flow Text
resolveAddress person leg lat lon mbAddress = case mfilter (not . T.null) (T.strip <$> mbAddress) of
  Just address -> pure address
  Nothing -> do
    res <- withTryCatch "shuttleInterest:address" (Maps.getPlaceName person.merchantId person.merchantOperatingCityId Nothing (Maps.GetPlaceNameReq (Maps.ByLatLong (Maps.LatLong lat lon)) Nothing Nothing))
    case either (const Nothing) (\places -> listToMaybe (mapMaybe (mfilter (not . T.null) . (.formattedAddress)) places)) res of
      Just address -> pure address
      Nothing -> throwError $ InvalidRequest ("Could not find the " <> leg <> " address; search for it and pick it from the list")
