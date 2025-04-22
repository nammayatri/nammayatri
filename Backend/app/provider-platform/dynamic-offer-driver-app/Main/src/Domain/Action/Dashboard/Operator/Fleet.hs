-- {-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.Operator.Fleet
  ( -- postFleetOperatorFleetRegister,
    postFleetOperatorFleetLink,
    postFleetOperatorFleetUnlink,
  )
where

import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as DomainRC
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person as DP
import qualified Environment
import Kernel.Beam.Functions as B
import Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.DriverFleetOperatorAssociation as SA
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.FleetOperatorAssociation as QFOA
import qualified Storage.Queries.Person as QP
import Tools.Error

-- postFleetOperatorFleetRegister :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Operator.Fleet.FleetOwnerRegisterReq -> Environment.Flow API.Types.ProviderPlatform.Operator.Fleet.FleetOwnerRegisterRes)
-- postFleetOperatorFleetRegister _merchantShortId _opCity requestorId req = do error "Logic yet to be decided" requestorId req

postFleetOperatorFleetLink ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Text ->
  Kernel.Prelude.Text ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postFleetOperatorFleetLink merchantShortId opCity fleetOwnerId requestorId = do
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

postFleetOperatorFleetUnlink ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Text ->
  Kernel.Prelude.Text ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postFleetOperatorFleetUnlink _merchantShortId _opCity fleetOwnerId requestorId = do
  operator <- B.runInReplica $ QP.findById (Id requestorId :: Id DP.Person) >>= fromMaybeM (PersonNotFound requestorId)
  unless (operator.role == DP.OPERATOR) $
    throwError AccessDenied

  fleetOwner <- B.runInReplica $ QP.findById (Id fleetOwnerId :: Id DP.Person) >>= fromMaybeM (FleetOwnerNotFound fleetOwnerId)
  unless (fleetOwner.role == DP.FLEET_OWNER) $
    throwError (InvalidRequest "Invalid fleet owner")

  QFOA.endFleetOperatorAssociation fleetOwner.id operator.id

  -- TODO send sms about unlink
  pure Kernel.Types.APISuccess.Success
