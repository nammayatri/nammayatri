module Domain.Action.Dashboard.Management.DriverVehicleQuality
  ( getDriverVehicleQualityList,
    getDriverVehicleQualitySearch,
    postDriverVehicleQualityUpdateVehicleRating,
  )
where

import qualified API.Types.ProviderPlatform.Management.DriverVehicleQuality as Common
import Data.Time (utctDay)
import Data.Time.Calendar (Day, addDays, diffDays)
import qualified Domain.Types.DriverStats as DStats
import qualified Domain.Types.FeedbackBadge as DFB
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Vehicle as DVeh
import qualified Domain.Types.VehicleVariant as DVariant
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt, getDbHash)
import qualified Kernel.Types.APISuccess as APISuccess
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.FeedbackBadgeExtra as QFeedbackBadge
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Vehicle as QVehicle
import qualified Storage.Queries.VehicleRegistrationCertificate as QVRC
import Tools.Error

getDriverVehicleQualityList ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe Int ->
  Maybe Double ->
  Maybe Int ->
  Int ->
  Double ->
  DVariant.VehicleVariant ->
  Environment.Flow Common.DriverVehicleQualityListRes
getDriverVehicleQualityList merchantShortId opCity mbLimit mbMaxVehicleRating mbOffset maxVehicleAge minDriverRating vehicleVariant = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  now <- getCurrentTime
  let today = utctDay now
      cutoffDate = addDays (fromIntegral (negate (maxVehicleAge * 365))) today
      limit = min 50 . fromMaybe 20 $ mbLimit
      offset = fromMaybe 0 mbOffset
      minRatingCentesimal = realToFrac minDriverRating

  vehicles <- B.runInReplica $ QVehicle.findEnabledByVariantCityAndManufacturingDateAfter vehicleVariant cutoffDate merchant.id merchantOpCity.id mbMaxVehicleRating limit offset
  let driverIds = map (.driverId) vehicles

  driverStatsList <- B.runInReplica $ QDriverStats.findAllByDriverIdList (cast <$> driverIds)
  let qualifiedDriverIds =
        [ dId
          | vehicle <- vehicles,
            let dId = vehicle.driverId,
            let mbStats = find (\ds -> ds.driverId == cast dId) driverStatsList,
            maybe False (\ds -> maybe False (>= minRatingCentesimal) ds.rating) mbStats
        ]

  drivers <- B.runInReplica $ QPerson.findAllByPersonIds (getId <$> qualifiedDriverIds)
  feedbackBadgesList <- B.runInReplica $ QFeedbackBadge.findAllByDriverIds qualifiedDriverIds

  items <- mapM (buildQualityResp today vehicles driverStatsList feedbackBadgesList) drivers
  pure Common.DriverVehicleQualityListRes {totalItems = length items, drivers = items}

getDriverVehicleQualitySearch ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe Text ->
  Maybe Text ->
  Environment.Flow [Common.DriverVehicleQualityResp]
getDriverVehicleQualitySearch merchantShortId opCity mbPhoneNumber mbVehicleNumber = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  now <- getCurrentTime
  let today = utctDay now

  driverIds <- case (mbPhoneNumber, mbVehicleNumber) of
    (Just phone, _) -> do
      phoneHash <- getDbHash phone
      mbPerson <- QPerson.findByMobileNumberAndMerchantAndRole "+91" phoneHash merchant.id DP.DRIVER
      case mbPerson of
        Just p | p.merchantOperatingCityId == merchantOpCity.id -> pure [p.id]
        _ -> pure []
    (_, Just vehNo) -> do
      mbVehicle <- QVehicle.findByRegistrationNo vehNo
      case mbVehicle of
        Just v | v.merchantId == merchant.id -> pure [v.driverId]
        _ -> pure []
    _ -> throwError $ InvalidRequest "Either phoneNumber or vehicleNumber must be provided"

  if null driverIds
    then pure []
    else do
      vehicleList <- catMaybes <$> (B.runInReplica $ mapM QVehicle.findById driverIds)
      driverStatsList <- B.runInReplica $ QDriverStats.findAllByDriverIdList (cast <$> driverIds)
      feedbackBadgesList <- B.runInReplica $ QFeedbackBadge.findAllByDriverIds driverIds
      drivers <- B.runInReplica $ QPerson.findAllByPersonIds (getId <$> driverIds)
      mapM (buildQualityResp today vehicleList driverStatsList feedbackBadgesList) drivers

postDriverVehicleQualityUpdateVehicleRating ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.UpdateVehicleRatingReq ->
  Environment.Flow APISuccess.APISuccess
postDriverVehicleQualityUpdateVehicleRating merchantShortId opCity req = do
  _merchant <- findMerchantByShortId merchantShortId
  _merchantOpCity <- CQMOC.findByMerchantIdAndCity _merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> _merchant.id.getId <> "-city-" <> show opCity)
  unless (req.rating >= 1 && req.rating <= 5) $
    throwError $ InvalidRequest "Rating must be between 1 and 5"

  mbRc <- QVRC.findLastVehicleRCWrapper req.registrationNo
  whenJust mbRc $ \rc -> QVRC.updateVehicleRatingAndRemark (Just req.rating) (Just req.remark) rc.id

  mbVehicle <- QVehicle.findByRegistrationNo req.registrationNo
  whenJust mbVehicle $ \vehicle ->
    QVehicle.updateVehicleRatingAndRemark (Just req.rating) (Just req.remark) vehicle.driverId

  logInfo $ "Vehicle rating updated for RC: " <> req.registrationNo <> " rating: " <> show req.rating
  pure APISuccess.Success

buildQualityResp ::
  (EncFlow m r) =>
  Day ->
  [DVeh.Vehicle] ->
  [DStats.DriverStats] ->
  [DFB.FeedbackBadge] ->
  DP.Person ->
  m Common.DriverVehicleQualityResp
buildQualityResp today vehicles driverStatsList feedbackBadgesList person = do
  let mbVehicle = find (\v -> v.driverId == person.id) vehicles
      mbStats = find (\ds -> ds.driverId == cast person.id) driverStatsList
      badges = filter (\fb -> fb.driverId == person.id) feedbackBadgesList
      vehicleAgeInMonths = mbVehicle >>= (.mYManufacturing) >>= \mfgDate -> Just $ fromIntegral (diffDays today mfgDate) `div` 30
  phoneNo <- mapM decrypt person.mobileNumber
  let driverName = person.firstName <> maybe "" (" " <>) person.lastName
  pure
    Common.DriverVehicleQualityResp
      { driverId = cast person.id,
        driverName = driverName,
        phoneNumber = phoneNo,
        vehicleAge = vehicleAgeInMonths,
        manufacturingDate = mbVehicle >>= (.mYManufacturing),
        driverRating = mbStats >>= (.rating),
        totalRides = maybe 0 (.totalRides) mbStats,
        feedbackBadges = map (\fb -> Common.FeedbackBadgeCount {badge = fb.badge, badgeCount = fb.badgeCount}) badges,
        vehicleVariant = maybe DVariant.SEDAN (.variant) mbVehicle,
        vehicleModel = maybe "" (.model) mbVehicle,
        vehicleMake = mbVehicle >>= (.make),
        fuelType = mbVehicle >>= (.energyType),
        vehicleColor = maybe "" (.color) mbVehicle,
        registrationNo = maybe "" (.registrationNo) mbVehicle,
        airConditioned = mbVehicle >>= (.airConditioned),
        vehicleName = mbVehicle >>= (.vehicleName),
        vehicleCapacity = mbVehicle >>= (.capacity),
        vehicleRating = mbVehicle >>= (.vehicleRating),
        vehicleRatingRemark = mbVehicle >>= (.vehicleRatingRemark)
      }
