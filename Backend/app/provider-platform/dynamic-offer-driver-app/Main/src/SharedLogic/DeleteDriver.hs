{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.DeleteDriver where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import Environment
import qualified IssueManagement.Storage.Queries.Issue.IssueReport as QIssueReport
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import SharedLogic.DriverOnboarding (isFleetRole)
import qualified SharedLogic.DriverOnboarding.OnboardingFlags.Guard as SGuard
import SharedLogic.Merchant (findMerchantByShortId)
import Storage.Beam.IssueManagement ()
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.AadhaarCard as QAadhaarCard
import qualified Storage.Queries.AadhaarOtpReq as AadhaarReq
import qualified Storage.Queries.AadhaarOtpVerify as AadhaarOtp
import qualified Storage.Queries.DailyStats as QDailyStats
import qualified Storage.Queries.DriverBankAccount as QDBA
import qualified Storage.Queries.DriverGstin as QDriverGstin
import qualified Storage.Queries.DriverIdentityInfo as QDriverIdentityInfo
import qualified Storage.Queries.DriverInformation as QDriverInfo
import qualified Storage.Queries.DriverLicense as QDriverLicense
import qualified Storage.Queries.DriverOperatorAssociation as QDriverOperatorAssociation
import qualified Storage.Queries.DriverPanCard as QPanCard
import qualified Storage.Queries.DriverQuote as QDriverQuote
import qualified Storage.Queries.DriverRCAssociation as QRCAssociation
import qualified Storage.Queries.DriverReferral as QDriverReferral
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.DriverUdyam as QDriverUdyam
import qualified Storage.Queries.FleetDriverAssociation as QFleetDriverAssociation
import qualified Storage.Queries.FleetOperatorAssociation as QFleetOperatorAssociation
import qualified Storage.Queries.FleetOwnerInformation as QFleetOwnerInformation
import qualified Storage.Queries.FleetRCAssociation as QFleetRCAssociation
import qualified Storage.Queries.IdfyVerification as QIV
import qualified Storage.Queries.Image as QImage
import qualified Storage.Queries.MessageReport as QMessage
import qualified Storage.Queries.OperationHubRequests as QOperationHubRequests
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.RegistrationToken as QR
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SearchRequestForDriver as QSearchReqForDriver
import qualified Storage.Queries.Vehicle as QVehicle
import qualified Storage.Queries.VehicleRegistrationCertificate as QVehicleRC
import qualified Tools.Auth as Auth
import Tools.Error

checkOperatorActiveAssociations :: Id DP.Person -> Flow ()
checkOperatorActiveAssociations personId = do
  QDriverOperatorAssociation.findActiveAssociationByOperatorId personId >>= flip whenJust (\_ -> throwError $ InvalidRequest "Cannot delete operator with active driver associations")
  QFleetOperatorAssociation.findActiveAssociationByOperatorId personId >>= flip whenJust (\_ -> throwError $ InvalidRequest "Cannot delete operator with active fleet associations")

deleteDriver :: ShortId DM.Merchant -> Id DP.Person -> Flow APISuccess
deleteDriver merchantShortId reqDriverId = do
  merchant <- findMerchantByShortId merchantShortId
  mbPerson <- QPerson.findById reqDriverId
  case mbPerson of
    Nothing -> do
      logTagError "deleteDriver" $ "Person does not exist at driver side with personId: " <> show reqDriverId
      return Success
    Just person -> do
      when (person.merchantId /= merchant.id) $
        throwError $ InvalidRequest "Person does not belong to this merchant"
      case person.role of
        role | isFleetRole role -> do
          transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = person.merchantOperatingCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound person.merchantOperatingCityId.getId)
          SGuard.guardOnboardingAction transporterConfig SGuard.None SGuard.Delete (SGuard.TargetFleetOwner person.id)
          QFleetOperatorAssociation.deleteByFleetOwnerId person.id.getId
          QFleetRCAssociation.deleteByFleetOwnerId person.id
          QFleetDriverAssociation.deleteByFleetOwnerId person.id.getId
          QVehicleRC.deleteByFleetOwnerId person.id.getId
          QAadhaarCard.deleteByPersonId person.id
          QPanCard.deleteByDriverId person.id
          QDriverGstin.deleteByDriverId person.id
          QDriverUdyam.deleteByDriverId person.id
          QDriverReferral.deleteByDriverId person.id
          QIV.deleteByPersonId person.id
          QImage.deleteByPersonId person.id
          QDBA.deleteById person.id
          QFleetOwnerInformation.deleteByFleetOwnerPersonId person.id
          QPerson.deleteById person.id
          logTagInfo "deleteFleet : " (show reqDriverId)
        DP.DRIVER -> do
          transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = person.merchantOperatingCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound person.merchantOperatingCityId.getId)
          driverDeleteCheck <- validateDriver merchant person (transporterConfig.unifiedOnboardingFlagsRecompute == Just True)
          when driverDeleteCheck $ throwError $ InvalidRequest "Driver can't be deleted"
          SGuard.guardOnboardingAction transporterConfig SGuard.None SGuard.Delete (SGuard.TargetDriver person.id)
          -- this function uses tokens from db, so should be called before transaction
          Auth.clearDriverSession person.id
          -- Esq.runTransaction $ do
          QIV.deleteByPersonId person.id
          QImage.deleteByPersonId person.id
          QDriverLicense.deleteByDriverId person.id
          QRCAssociation.deleteByDriverId person.id
          QDriverQuote.deleteByDriverId person.id
          QSearchReqForDriver.deleteByDriverId person.id
          QFleetDriverAssociation.deleteByDriverId person.id
          QDriverOperatorAssociation.deleteByDriverId person.id
          QDriverReferral.deleteByDriverId person.id
          QOperationHubRequests.deleteByDriverId person.id
          QDriverStats.deleteById (cast person.id)
          QDailyStats.deleteAllByDriverId person.id
          QR.deleteByPersonId person.id.getId
          QVehicle.deleteById person.id
          QDriverInfo.deleteById (cast person.id)
          QMessage.deleteByPersonId person.id
          QIssueReport.deleteByPersonId (cast person.id)
          AadhaarReq.deleteByPersonId person.id
          AadhaarOtp.deleteByPersonId person.id
          QAadhaarCard.deleteByPersonId person.id
          QPanCard.deleteByDriverId person.id
          QDriverIdentityInfo.deleteByDriverId person.id
          QDBA.deleteById person.id
          QPerson.deleteById person.id
          logTagInfo "deleteDriver : " (show reqDriverId)
        DP.OPERATOR -> do
          checkOperatorActiveAssociations person.id
          QOperationHubRequests.deleteByOperatorId person.id
          QDriverOperatorAssociation.deleteByOperatorId person.id
          QFleetOperatorAssociation.deleteByOperatorId person.id
          QDriverReferral.deleteByDriverId person.id
          QIV.deleteByPersonId person.id
          QImage.deleteByPersonId person.id
          QPerson.deleteById person.id
          logTagInfo "deleteOperator : " (show reqDriverId)
        _ -> pure ()
      return Success

validateDriver :: (EsqDBFlow m r, EncFlow m r, CacheFlow m r) => DM.Merchant -> DP.Person -> Bool -> m Bool
validateDriver merchant driver unifiedRecompute = do
  let personId = driver.id
  ride <- QRide.findOneByDriverId personId
  driverInformation <- QDriverInfo.findById (cast personId) >>= fromMaybeM DriverInfoNotFound
  let stillActive = driverInformation.enabled && (not unifiedRecompute || isNothing driverInformation.disabledReasonFlag)
  return (merchant.id /= driver.merchantId || driver.role /= DP.DRIVER || isJust ride || stillActive)
