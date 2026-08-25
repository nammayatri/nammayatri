module Domain.Action.Dashboard.Management.Vehicle (getVehicleList, getVehicleInfo) where

import qualified API.Types.ProviderPlatform.Management.Vehicle as VehicleAPI
import qualified Dashboard.Common as Common
import qualified Dashboard.Common.Driver as Common
import qualified Data.HashMap.Strict as HM
import Data.List (nub)
import qualified Domain.Action.Dashboard.Common as DCommon
import qualified Domain.Types.DriverRCAssociation as DDRCA
import qualified Domain.Types.FleetOwnerInformation as DFOI
import qualified Domain.Types.FleetRCAssociation as DFRCA
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.VehicleRegistrationCertificate as DVRC
import Environment
import qualified Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt, getDbHash)
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified SharedLogic.DriverOnboarding as SDO
import qualified SharedLogic.DriverOnboarding.OnboardingFlags.Guard as SGuard
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.DriverRCAssociationExtra as QDRCA
import qualified Storage.Queries.FleetOwnerInformation as QFOI
import qualified Storage.Queries.FleetRCAssociationExtra as QFRCA
import qualified Storage.Queries.OnboardingList.VehicleList as QVehicleList
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.PersonExtra as QPersonExtra
import qualified Storage.Queries.VehicleRegistrationCertificate as RCQuery
import Tools.Auth ()
import Tools.Error

getVehicleList ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe Int ->
  Maybe Int ->
  Maybe Text ->
  Maybe Text ->
  Maybe Bool ->
  Maybe Common.ApprovalStatusFilter ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe Text ->
  Flow VehicleAPI.VehicleListRes
getVehicleList merchantShortId opCity mbLimit mbOffset mbFleetOwnerId mbVehicleNumber mbVerified mbApprovalStatus mbFrom mbTo mbRequestorId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  mbRequestor <- case mbRequestorId of
    Just requestorId -> B.runInReplica $ QPerson.findById (Id requestorId :: Id DP.Person)
    Nothing -> pure Nothing
  let scopedFleetOwnerId = case mbRequestor of
        Just person | SDO.isFleetRole person.role -> Just person.id.getId
        _ -> mbFleetOwnerId
      limit = min maxLimit . fromMaybe defaultLimit $ mbLimit
      offset = fromMaybe 0 mbOffset
      mbApprovalFilter = approvalStatusToFilter <$> mbApprovalStatus
  mbCertificateNumberHash <- getDbHash `traverse` mbVehicleNumber
  rcs <- B.runInReplica $ QVehicleList.findVehicles merchant.id merchantOpCity.id limit offset scopedFleetOwnerId mbCertificateNumberHash mbVerified mbApprovalFilter mbFrom mbTo
  vehicles <- buildVehicleListItems rcs
  let count = length vehicles
  let summary = Common.Summary {totalCount = 10000, count}
  pure VehicleAPI.VehicleListRes {totalItems = count, summary, vehicles}
  where
    maxLimit = 50
    defaultLimit = 10

getVehicleInfo :: ShortId DM.Merchant -> Context.City -> Text -> Bool -> Maybe Text -> Flow VehicleAPI.VehicleListItem
getVehicleInfo merchantShortId opCity fleetOwnerId mbFleet mbVehicleNumber = do
  vehicleNumber <- fromMaybeM (InvalidRequest "vehicleNumber is required") mbVehicleNumber
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  rc <- B.runInReplica (RCQuery.findLastVehicleRCWrapper vehicleNumber) >>= fromMaybeM (VehicleNotFoundForNumber vehicleNumber)
  -- The list query scopes to merchant and city; the point lookup does not, so scope here.
  unless (rc.merchantId == Just merchant.id && rc.merchantOperatingCityId == Just merchantOpCity.id) $
    throwError (VehicleNotFoundForNumber vehicleNumber)
  when mbFleet $ do
    transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCity.id.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCity.id.getId)
    SGuard.guardOnboardingAction transporterConfig (SGuard.ActorFleet (Id fleetOwnerId)) SGuard.View (SGuard.TargetVehicleById rc.id)
  listToMaybe <$> buildVehicleListItems [rc] >>= fromMaybeM (VehicleNotFoundForNumber vehicleNumber)

buildVehicleListItems :: [DVRC.VehicleRegistrationCertificate] -> Flow [VehicleAPI.VehicleListItem]
buildVehicleListItems rcs = do
  let rcIds = map (.id) rcs
  fleetAssocs <- B.runInReplica $ QFRCA.findAllActiveByRcIds rcIds
  driverAssocs <- B.runInReplica $ QDRCA.findAllActiveByRcIds rcIds
  let fleetAssocByRc = HM.fromListWith (\_ existing -> existing) $ map (\fra -> (fra.rcId, fra)) fleetAssocs
      driverAssocByRc = HM.fromListWith (\_ existing -> existing) $ map (\dra -> (dra.rcId, dra)) driverAssocs
      fleetOwnerIds = nub $ map (.fleetOwnerId) fleetAssocs
      driverIds = nub $ map (.driverId) driverAssocs
  persons <- B.runInReplica $ QPersonExtra.findAllByPersonIds (map getId (fleetOwnerIds <> driverIds))
  fleetOwnerInfos <- B.runInReplica $ QFOI.findAllByPrimaryKeys fleetOwnerIds
  let personById = HM.fromList $ map (\p -> (p.id, p)) persons
      fleetOwnerInfoById = HM.fromList $ map (\foi -> (foi.fleetOwnerPersonId, foi)) fleetOwnerInfos
  mapM (buildVehicleListItem fleetAssocByRc driverAssocByRc personById fleetOwnerInfoById) rcs

approvalStatusToFilter :: Common.ApprovalStatusFilter -> Maybe Bool
approvalStatusToFilter Common.ApprovedOnly = Just True
approvalStatusToFilter Common.RejectedOnly = Just False
approvalStatusToFilter Common.PendingOnly = Nothing

buildVehicleListItem ::
  HM.HashMap (Id DVRC.VehicleRegistrationCertificate) DFRCA.FleetRCAssociation ->
  HM.HashMap (Id DVRC.VehicleRegistrationCertificate) DDRCA.DriverRCAssociation ->
  HM.HashMap (Id DP.Person) DP.Person ->
  HM.HashMap (Id DP.Person) DFOI.FleetOwnerInformation ->
  DVRC.VehicleRegistrationCertificate ->
  Flow VehicleAPI.VehicleListItem
buildVehicleListItem fleetAssocByRc driverAssocByRc personById fleetOwnerInfoById rc = do
  vehicleNumber <- decrypt rc.certificateNumber
  recentFleetInfo <- case HM.lookup rc.id fleetAssocByRc of
    Nothing -> pure Nothing
    Just fra -> mkAssociationInfo (HM.lookup fra.fleetOwnerId personById) (HM.lookup fra.fleetOwnerId fleetOwnerInfoById) fra.associatedTill True
  linkedDriverInfo <- case HM.lookup rc.id driverAssocByRc of
    Nothing -> pure Nothing
    Just dra -> mkAssociationInfo (HM.lookup dra.driverId personById) Nothing dra.associatedTill dra.isRcActive
  pure
    VehicleAPI.VehicleListItem
      { rcId = rc.id.getId,
        vehicleNumber = Just vehicleNumber,
        vehicleMake = rc.vehicleManufacturer,
        vehicleModel = rc.vehicleModel,
        vehicleColor = rc.vehicleColor,
        vehicleClass = rc.vehicleClass,
        vehicleVariant = DCommon.castVehicleVariantDashboard rc.vehicleVariant,
        verified = rc.verified,
        approved = rc.approved,
        createdAt = rc.createdAt,
        recentFleetInfo,
        linkedDriverInfo
      }

mkAssociationInfo :: Maybe DP.Person -> Maybe DFOI.FleetOwnerInformation -> Maybe UTCTime -> Bool -> Flow (Maybe Common.DriverAssociationInfo)
mkAssociationInfo Nothing _ _ _ = pure Nothing
mkAssociationInfo (Just person) mbFoi associatedTill isActive = do
  mobileNumber <- mapM decrypt person.mobileNumber
  now <- getCurrentTime
  pure $
    Just
      Common.DriverAssociationInfo
        { personId = cast @DP.Person @Common.Person person.id,
          name = Just $ person.firstName <> maybe "" (" " <>) person.lastName,
          mobileCountryCode = person.mobileCountryCode,
          mobileNumber,
          fleetName = mbFoi >>= (.fleetName),
          verified = (.verified) <$> mbFoi,
          enabled = (.enabled) <$> mbFoi,
          isActive,
          isAssociated = maybe False (> now) associatedTill,
          associatedTill,
          requestReason = Nothing
        }
