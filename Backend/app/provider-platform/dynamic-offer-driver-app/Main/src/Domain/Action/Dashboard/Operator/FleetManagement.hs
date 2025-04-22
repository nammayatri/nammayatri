module Domain.Action.Dashboard.Operator.FleetManagement
  ( getFleetManagementFleets,
    postFleetManagementFleetRegister,
    postFleetManagementFleetCreate,
    postFleetManagementFleetLink,
    postFleetManagementFleetUnlink,
  )
where

import qualified API.Types.ProviderPlatform.Operator.FleetManagement as Common
import Control.Monad.Extra (mapMaybeM)
import qualified Domain.Action.Dashboard.Fleet.Registration as DRegistration
import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as DomainRC
import Domain.Types.FleetOperatorAssociation (FleetOperatorAssociation (fleetOwnerId))
import qualified Domain.Types.FleetOwnerInformation as FOI
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person as DP
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt)
import Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Id
import qualified Kernel.Types.Id as ID
import Kernel.Utils.Common
import qualified SharedLogic.DriverFleetOperatorAssociation as SA
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.FleetOperatorAssociation as QFOA
import Storage.Queries.FleetOperatorAssociationExtra (findAllActiveByOperatorIdWithLimitOffset)
import Storage.Queries.FleetOwnerInformationExtra
  ( findByPersonIdAndEnabledAndVerified,
  )
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.VehicleRegistrationCertificateExtra as VRCQuery
import Tools.Error

getFleetManagementFleets ::
  ID.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
  Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Text ->
  Environment.Flow [Common.FleetInfo]
getFleetManagementFleets _merchantShortId _opCity mbIsActive mbVerified mbLimit mbOffset requestorId = do
  person <- QP.findById (ID.Id requestorId) >>= fromMaybeM (PersonNotFound requestorId)
  unless (person.role == DP.OPERATOR) $ throwError (InvalidRequest "Requestor role is not OPERATOR")
  activeFleetOwnerLs <- findAllActiveByOperatorIdWithLimitOffset requestorId mbLimit mbOffset
  fleetOwnerInfoLs <-
    mapMaybeM (findByPersonIdAndEnabledAndVerified mbIsActive mbVerified . ID.Id . fleetOwnerId) activeFleetOwnerLs
  mapM createFleetInfo fleetOwnerInfoLs
  where
    createFleetInfo FOI.FleetOwnerInformation {..} = do
      totalVehicle <- VRCQuery.countAllActiveRCForFleet fleetOwnerPersonId.getId merchantId
      person <-
        QP.findById fleetOwnerPersonId
          >>= fromMaybeM (PersonDoesNotExist fleetOwnerPersonId.getId)
      decryptedMobileNumber <-
        mapM decrypt person.mobileNumber
          >>= fromMaybeM (InvalidRequest $ "Person do not have a mobile number " <> person.id.getId)
      pure $
        Common.FleetInfo
          { id = ID.cast fleetOwnerPersonId,
            name = person.firstName,
            isActive = enabled,
            mobileCountryCode = fromMaybe "+91" person.mobileCountryCode,
            mobileNumber = decryptedMobileNumber,
            vehicleCount = totalVehicle,
            verified = verified
          }

postFleetManagementFleetCreate ::
  ID.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Text ->
  Common.FleetOwnerCreateReq ->
  Environment.Flow Common.FleetOwnerCreateRes
postFleetManagementFleetCreate merchantShortId opCity requestorId req = do
  let enabled = Just True
  mkFleetOwnerRegisterRes <$> DRegistration.fleetOwnerLogin (Just requestorId) enabled (mkFleetOwnerLoginReq merchantShortId opCity req)

mkFleetOwnerLoginReq ::
  ID.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.FleetOwnerCreateReq ->
  DRegistration.FleetOwnerLoginReq
mkFleetOwnerLoginReq merchantShortId opCity (Common.FleetOwnerCreateReq {..}) = do
  DRegistration.FleetOwnerLoginReq
    { otp = Nothing,
      merchantId = merchantShortId.getShortId,
      city = opCity,
      ..
    }

mkFleetOwnerRegisterRes ::
  DRegistration.FleetOwnerRegisterRes ->
  Common.FleetOwnerCreateRes
mkFleetOwnerRegisterRes DRegistration.FleetOwnerRegisterRes {..} =
  Common.FleetOwnerCreateRes {personId = ID.Id personId}

postFleetManagementFleetRegister ::
  ID.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Text ->
  Common.FleetOwnerRegisterReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postFleetManagementFleetRegister merchantShortId opCity requestorId req =
  DRegistration.fleetOwnerRegister (Just requestorId) $ mkFleetOwnerRegisterReq merchantShortId opCity req

mkFleetOwnerRegisterReq ::
  ID.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.FleetOwnerRegisterReq ->
  DRegistration.FleetOwnerRegisterReq
mkFleetOwnerRegisterReq merchantShortId opCity (Common.FleetOwnerRegisterReq {..}) = do
  DRegistration.FleetOwnerRegisterReq
    { personId = ID.cast @Common.Person @DP.Person personId,
      fleetType = castFleetType <$> fleetType,
      operatorReferralCode = Nothing,
      merchantId = merchantShortId.getShortId,
      city = opCity,
      ..
    }

castFleetType :: Common.FleetType -> FOI.FleetType
castFleetType = \case
  Common.RENTAL_FLEET -> FOI.RENTAL_FLEET
  Common.NORMAL_FLEET -> FOI.NORMAL_FLEET
  Common.BUSINESS_FLEET -> FOI.BUSINESS_FLEET

postFleetManagementFleetLink ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Text ->
  Kernel.Prelude.Text ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postFleetManagementFleetLink merchantShortId opCity fleetOwnerId requestorId = do
  operator <- B.runInReplica $ QP.findById (Id requestorId :: Id DP.Person) >>= fromMaybeM (PersonNotFound requestorId)
  unless (operator.role == DP.OPERATOR) $
    throwError AccessDenied

  fleetOwner <- B.runInReplica $ QP.findById (Id fleetOwnerId :: Id DP.Person) >>= fromMaybeM (FleetOwnerNotFound fleetOwnerId)
  unless (fleetOwner.role == DP.FLEET_OWNER) $
    throwError (InvalidRequest "Invalid fleet owner")

  checkAssocOperator <- B.runInReplica $ QFOA.findByFleetOwnerIdAndOperatorId fleetOwner.id operator.id True
  when (isJust checkAssocOperator) $ throwError (InvalidRequest "Fleet already associated with operator")

  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  SA.endFleetAssociationsIfAllowed merchant merchantOpCityId fleetOwner

  fleetOperatorAssociation <- SA.makeFleetOperatorAssociation merchant.id merchantOpCityId fleetOwnerId operator.id.getId (DomainRC.convertTextToUTC (Just "2099-12-12"))
  QFOA.create fleetOperatorAssociation

  -- TODO send otp or joining message sms
  pure Kernel.Types.APISuccess.Success

postFleetManagementFleetUnlink ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Text ->
  Kernel.Prelude.Text ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postFleetManagementFleetUnlink _merchantShortId _opCity fleetOwnerId requestorId = do
  operator <- B.runInReplica $ QP.findById (Id requestorId :: Id DP.Person) >>= fromMaybeM (PersonNotFound requestorId)
  unless (operator.role == DP.OPERATOR) $
    throwError AccessDenied

  fleetOwner <- B.runInReplica $ QP.findById (Id fleetOwnerId :: Id DP.Person) >>= fromMaybeM (FleetOwnerNotFound fleetOwnerId)
  unless (fleetOwner.role == DP.FLEET_OWNER) $
    throwError (InvalidRequest "Invalid fleet owner")

  QFOA.endFleetOperatorAssociation fleetOwner.id operator.id

  -- TODO send sms about unlink
  pure Kernel.Types.APISuccess.Success
