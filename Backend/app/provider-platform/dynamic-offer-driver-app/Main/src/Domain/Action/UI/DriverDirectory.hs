module Domain.Action.UI.DriverDirectory where

import qualified API.Types.UI.DriverDirectory as API
import qualified Domain.Types.DriverDirectoryProfile as DTDDP
import qualified Domain.Types.DriverStats as DTS
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Vehicle as DVeh
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common as KUC
import qualified Storage.Queries.DriverDirectoryProfile as QDDP
import qualified Storage.Queries.DriverStats as QDS
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Vehicle as QVeh

postDriverDirectoryOptIn ::
  ( ( Maybe (Id SP.Person),
      Id DM.Merchant,
      Id DMOC.MerchantOperatingCity
    ) ->
    API.DriverDirectoryOptInReq ->
    Flow APISuccess
  )
postDriverDirectoryOptIn (mbPersonId, merchantId, merchantOpCityId) req = do
  driverId <- mbPersonId & fromMaybeM (PersonNotFound "No person id passed")
  _person <- QP.findById driverId >>= fromMaybeM (PersonNotFound ("No person found with id " <> show driverId))
  now <- getCurrentTime
  existingProfile <- QDDP.findByDriverId driverId
  case existingProfile of
    Just profile -> do
      QDDP.updateIsListedByDriverId True (Just now) now driverId
      QDDP.updateProfileByDriverId
        (req.preferredVehicleType <|> profile.preferredVehicleType)
        (if null req.preferredCities then profile.preferredCities else req.preferredCities)
        (req.yearsOfExperience <|> profile.yearsOfExperience)
        (req.bio <|> profile.bio)
        req.showPhoneNumber
        req.showRating
        req.showTotalRides
        req.showVehicleInfo
        profile.availabilityStatus
        now
        driverId
    Nothing -> do
      let newProfile =
            DTDDP.DriverDirectoryProfile
              { driverId = driverId,
                isListed = True,
                listedAt = Just now,
                preferredVehicleType = req.preferredVehicleType,
                preferredCities = req.preferredCities,
                yearsOfExperience = req.yearsOfExperience,
                bio = req.bio,
                showPhoneNumber = req.showPhoneNumber,
                showRating = req.showRating,
                showTotalRides = req.showTotalRides,
                showVehicleInfo = req.showVehicleInfo,
                availabilityStatus = DTDDP.AVAILABLE,
                merchantId = merchantId,
                merchantOperatingCityId = merchantOpCityId,
                createdAt = now,
                updatedAt = now
              }
      QDDP.create newProfile
  pure Success

postDriverDirectoryOptOut ::
  ( ( Maybe (Id SP.Person),
      Id DM.Merchant,
      Id DMOC.MerchantOperatingCity
    ) ->
    Flow APISuccess
  )
postDriverDirectoryOptOut (mbPersonId, _merchantId, _merchantOpCityId) = do
  driverId <- mbPersonId & fromMaybeM (PersonNotFound "No person id passed")
  _person <- QP.findById driverId >>= fromMaybeM (PersonNotFound ("No person found with id " <> show driverId))
  now <- getCurrentTime
  existingProfile <- QDDP.findByDriverId driverId
  case existingProfile of
    Just _ -> QDDP.updateIsListedByDriverId False Nothing now driverId
    Nothing -> logInfo "Driver directory profile not found, nothing to opt out from"
  pure Success

getDriverDirectoryStatus ::
  ( ( Maybe (Id SP.Person),
      Id DM.Merchant,
      Id DMOC.MerchantOperatingCity
    ) ->
    Flow API.DriverDirectoryStatusRes
  )
getDriverDirectoryStatus (mbPersonId, _merchantId, _merchantOpCityId) = do
  driverId <- mbPersonId & fromMaybeM (PersonNotFound "No person id passed")
  _person <- QP.findById driverId >>= fromMaybeM (PersonNotFound ("No person found with id " <> show driverId))
  mbProfile <- QDDP.findByDriverId driverId
  let completeness = calculateCompleteness mbProfile
  case mbProfile of
    Just profile ->
      pure
        API.DriverDirectoryStatusRes
          { isListed = profile.isListed,
            listedAt = profile.listedAt,
            profileCompleteness = completeness,
            preferredVehicleType = profile.preferredVehicleType,
            preferredCities = profile.preferredCities,
            yearsOfExperience = profile.yearsOfExperience,
            bio = profile.bio,
            showPhoneNumber = profile.showPhoneNumber,
            showRating = profile.showRating,
            showTotalRides = profile.showTotalRides,
            showVehicleInfo = profile.showVehicleInfo
          }
    Nothing ->
      pure
        API.DriverDirectoryStatusRes
          { isListed = False,
            listedAt = Nothing,
            profileCompleteness = 0,
            preferredVehicleType = Nothing,
            preferredCities = [],
            yearsOfExperience = Nothing,
            bio = Nothing,
            showPhoneNumber = False,
            showRating = True,
            showTotalRides = True,
            showVehicleInfo = True
          }

putDriverDirectoryProfile ::
  ( ( Maybe (Id SP.Person),
      Id DM.Merchant,
      Id DMOC.MerchantOperatingCity
    ) ->
    API.DriverDirectoryProfileUpdateReq ->
    Flow APISuccess
  )
putDriverDirectoryProfile (mbPersonId, _merchantId, _merchantOpCityId) req = do
  driverId <- mbPersonId & fromMaybeM (PersonNotFound "No person id passed")
  _person <- QP.findById driverId >>= fromMaybeM (PersonNotFound ("No person found with id " <> show driverId))
  profile <- QDDP.findByDriverId driverId >>= fromMaybeM (PersonNotFound ("No directory profile found for driver " <> show driverId))
  now <- getCurrentTime
  let updatedAvailability = maybe profile.availabilityStatus parseAvailability req.availabilityStatus
  QDDP.updateProfileByDriverId
    (req.preferredVehicleType <|> profile.preferredVehicleType)
    (fromMaybe profile.preferredCities req.preferredCities)
    (req.yearsOfExperience <|> profile.yearsOfExperience)
    (req.bio <|> profile.bio)
    (fromMaybe profile.showPhoneNumber req.showPhoneNumber)
    (fromMaybe profile.showRating req.showRating)
    (fromMaybe profile.showTotalRides req.showTotalRides)
    (fromMaybe profile.showVehicleInfo req.showVehicleInfo)
    updatedAvailability
    now
    driverId
  pure Success

getFleetDirectorySearch ::
  ( ( Maybe (Id SP.Person),
      Id DM.Merchant,
      Id DMOC.MerchantOperatingCity
    ) ->
    Maybe Text ->
    Maybe Int ->
    Maybe Text ->
    Maybe Text ->
    Maybe Text ->
    Maybe Int ->
    Maybe Int ->
    Flow API.DirectorySearchRes
  )
getFleetDirectorySearch (_mbPersonId, _merchantId, merchantOpCityId) mbCity mbMinRating mbVehicleType mbAvailability mbSearchQuery mbLimit mbOffset = do
  let limit' = fromMaybe 20 mbLimit
      offset' = fromMaybe 0 mbOffset
  (profiles, totalCount) <- QDDP.searchListedDrivers merchantOpCityId mbCity mbMinRating mbVehicleType mbAvailability mbSearchQuery limit' offset'
  driverItems <- mapM buildDriverListItem profiles
  pure
    API.DirectorySearchRes
      { drivers = driverItems,
        totalCount = totalCount,
        limit = limit',
        offset = offset'
      }

getFleetDirectoryDriverDriverId ::
  ( ( Maybe (Id SP.Person),
      Id DM.Merchant,
      Id DMOC.MerchantOperatingCity
    ) ->
    Id SP.Person ->
    Flow API.DirectoryDriverDetailRes
  )
getFleetDirectoryDriverDriverId (_mbPersonId, _merchantId, _merchantOpCityId) targetDriverId = do
  profile <- QDDP.findByDriverId targetDriverId >>= fromMaybeM (PersonNotFound ("No directory profile found for driver " <> show targetDriverId))
  person <- QP.findById targetDriverId >>= fromMaybeM (PersonNotFound ("No person found with id " <> show targetDriverId))
  mbDriverStats <- QDS.findByPrimaryKey targetDriverId
  mbVehicle <- QVeh.findById targetDriverId
  let cityName = Nothing -- Can be resolved from merchantOperatingCityId if needed
  pure
    API.DirectoryDriverDetailRes
      { driverId = targetDriverId,
        name = person.firstName <> maybe "" (" " <>) person.lastName,
        rating = mbDriverStats >>= (.rating),
        totalRides = (.totalRides) <$> mbDriverStats,
        yearsOfExperience = profile.yearsOfExperience,
        preferredVehicleType = profile.preferredVehicleType,
        preferredCities = profile.preferredCities,
        city = cityName,
        bio = profile.bio,
        listedAt = profile.listedAt,
        phoneNumber = if profile.showPhoneNumber then person.mobileNumber else Nothing,
        vehicleInfo = if profile.showVehicleInfo then (.model) =<< mbVehicle else Nothing,
        availabilityStatus = Just $ show profile.availabilityStatus
      }

-- Internal helpers

buildDriverListItem :: DTDDP.DriverDirectoryProfile -> Flow API.DirectoryDriverListItem
buildDriverListItem profile = do
  person <- QP.findById profile.driverId >>= fromMaybeM (PersonNotFound ("No person found with id " <> show profile.driverId))
  mbDriverStats <- QDS.findByPrimaryKey profile.driverId
  mbVehicle <- QVeh.findById profile.driverId
  pure
    API.DirectoryDriverListItem
      { driverId = profile.driverId,
        name = person.firstName <> maybe "" (" " <>) person.lastName,
        rating = mbDriverStats >>= (.rating),
        totalRides = (.totalRides) <$> mbDriverStats,
        yearsOfExperience = profile.yearsOfExperience,
        preferredVehicleType = profile.preferredVehicleType,
        city = Nothing,
        bio = profile.bio,
        listedAt = profile.listedAt,
        phoneNumber = if profile.showPhoneNumber then person.mobileNumber else Nothing,
        vehicleInfo = if profile.showVehicleInfo then (.model) =<< mbVehicle else Nothing
      }

calculateCompleteness :: Maybe DTDDP.DriverDirectoryProfile -> Int
calculateCompleteness Nothing = 0
calculateCompleteness (Just profile) =
  let fields =
        [ isJust profile.preferredVehicleType,
          not (null profile.preferredCities),
          isJust profile.yearsOfExperience,
          isJust profile.bio
        ]
      filledCount = length $ filter id fields
      totalFields = length fields
   in if totalFields == 0 then 0 else (filledCount * 100) `div` totalFields

parseAvailability :: Text -> DTDDP.AvailabilityStatus
parseAvailability "AVAILABLE" = DTDDP.AVAILABLE
parseAvailability "BUSY" = DTDDP.BUSY
parseAvailability "NOT_AVAILABLE" = DTDDP.NOT_AVAILABLE
parseAvailability _ = DTDDP.AVAILABLE
