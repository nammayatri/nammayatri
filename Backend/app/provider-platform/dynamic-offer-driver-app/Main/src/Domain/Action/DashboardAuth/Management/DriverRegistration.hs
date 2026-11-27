{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Hand-written handlers for direct-dashboard routes whose request needs more
-- than the verified operator's id or name: fleet-owner resolution, fleet-owner
-- verification, dashboard-database writes after the call, and similar.
--
-- provider-dashboard did this work in its own hand-written
-- @Domain.Action.ProviderPlatform.*@ layer before forwarding the call. The
-- generated @API.Action.DashboardAuth@ handler calls these functions instead of
-- the domain handler for every endpoint marked @appServerHandler: custom@ in
-- the spec, so this logic lives here and is never overwritten by the generator.
module Domain.Action.DashboardAuth.Management.DriverRegistration
  ( postDriverRegistrationDocumentUpload,
    postDriverRegistrationDocumentsUpdate,
    postDriverRegistrationUnlinkDocument,
    postDriverRegistrationTriggerReminder,
    getDriverRegistrationDocumentsList,
    getDriverRegistrationGetDocument,
  )
where

import qualified API.Types.ProviderPlatform.Management.DriverRegistration
import qualified Dashboard.Common
import qualified Domain.Action.Dashboard.Management.DriverRegistration
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.External.Payout.Interface.Types
import qualified Kernel.External.Verification.Interface.Types
import qualified Kernel.External.Verification.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified SharedLogic.FleetScope
import Tools.Auth
import Tools.Auth.DashboardUserAuth

-- | In ENFORCE a fleet owner or operator may list only the documents of people and vehicles that are theirs. Staff
-- who read documents by city or merchant are authorised by capability, as before.
getDriverRegistrationDocumentsList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.DriverRegistration.DocumentType -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.Flow API.Types.ProviderPlatform.Management.DriverRegistration.DocumentsListResponse)
getDriverRegistrationDocumentsList a6 a5 a4 a3 a2 a1 = do
  SharedLogic.FleetScope.assertMayReadDocumentList a4 a3.getId a1
  Domain.Action.Dashboard.Management.DriverRegistration.getDriverRegistrationDocumentsList a6 a5 a3 a2 a1

-- | As 'getDriverRegistrationDocumentsList', for one document, which is checked once it is known whose it is.
getDriverRegistrationGetDocument :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.DriverRegistration.DocumentType -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.DriverRegistration.EntityType -> Environment.Flow API.Types.ProviderPlatform.Management.DriverRegistration.GetDocumentResponse)
getDriverRegistrationGetDocument a6 a5 a4 a3 a2 a1 =
  Domain.Action.Dashboard.Management.DriverRegistration.getDriverRegistrationGetDocumentChecked (SharedLogic.FleetScope.assertMayReadDocument a4) a6 a5 a3 a2 a1

postDriverRegistrationDocumentUpload :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.DriverRegistration.UploadDocumentReq -> Environment.Flow API.Types.ProviderPlatform.Management.DriverRegistration.UploadDocumentResp)
postDriverRegistrationDocumentUpload a5 a4 a3 a2 a1 = do
  mbRequestorId <- driverRequestorId a3 a2
  Domain.Action.Dashboard.Management.DriverRegistration.postDriverRegistrationDocumentUpload a5 a4 a2 (a1 :: API.Types.ProviderPlatform.Management.DriverRegistration.UploadDocumentReq) {API.Types.ProviderPlatform.Management.DriverRegistration.requestorId = mbRequestorId}

postDriverRegistrationDocumentsUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.DriverRegistration.UpdateDocumentRequest -> Environment.Flow API.Types.ProviderPlatform.Management.DriverRegistration.UpdateDocumentResp)
postDriverRegistrationDocumentsUpdate a4 a3 _a2 a1 = do
  res <- Domain.Action.Dashboard.Management.DriverRegistration.postDriverRegistrationDocumentsUpdate a4 a3 a1
  Kernel.Prelude.whenJust res.personId $ \personId -> Tools.Auth.DashboardUserAuth.updateDashboardPersonVerified personId.getId res.enabled
  Kernel.Prelude.pure res

postDriverRegistrationUnlinkDocument :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.DriverRegistration.DocumentType -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postDriverRegistrationUnlinkDocument a5 a4 a3 a2 a1 = do
  mbRequestorId <- driverRequestorId a3 a2
  res <- Domain.Action.Dashboard.Management.DriverRegistration.postDriverRegistrationUnlinkDocument a5 a4 a2 a1 mbRequestorId
  Kernel.Prelude.when res.mandatoryDocumentRemoved $
    Tools.Auth.DashboardUserAuth.updateDashboardPersonVerified a2.getId Kernel.Prelude.False
  Kernel.Prelude.pure Kernel.Types.APISuccess.Success

postDriverRegistrationTriggerReminder :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.DriverRegistration.TriggerReminderReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postDriverRegistrationTriggerReminder a5 a4 a3 a2 a1 = do
  mbRequestorId <- driverRequestorId a3 a2
  Domain.Action.Dashboard.Management.DriverRegistration.postDriverRegistrationTriggerReminder a5 a4 a2 mbRequestorId a1

-- | The requestor the shared driver-registration functions check the driver's association against. Once fleet
-- scope is enforced the driver is checked against the fleets in the caller's scope instead, and no requestor is
-- passed, so the merchant's fleet-member-hierarchy flag is not consulted. Otherwise the old rule applies.
driverRequestorId :: DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.Flow (Kernel.Prelude.Maybe Kernel.Prelude.Text)
driverRequestorId dashboardUser driverId
  | SharedLogic.FleetScope.isEnforced dashboardUser = do
    SharedLogic.FleetScope.assertSubjectInScope dashboardUser driverId.getId
    Kernel.Prelude.pure Kernel.Prelude.Nothing
  | Kernel.Prelude.otherwise = Kernel.Prelude.pure (Tools.Auth.DashboardUserAuth.dashboardRequestorIdForDriver dashboardUser driverId.getId)
