{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Hand-written handlers for the direct-dashboard ticket-onboarding routes.
--
-- Every one of these carries a requestorId and a requestorRole. rider-dashboard
-- filled both from the session before forwarding
-- (@Domain.Action.RiderPlatform.AppManagement.MerchantOnboarding@) and the
-- application server trusts them -- it gates approval on the role alone. Taking
-- either from the client would let any caller claim to be an approver, so both
-- are resolved here instead.
module Domain.Action.DashboardAuth.AppManagement.MerchantOnboarding
  ( merchantOnboardingInfo,
    merchantOnboardingStart,
    merchantOnboardingList,
    merchantOnboardingStepSubmit,
    merchantOnboardingStepUpdatePayload,
    merchantOnboardingStepApprove,
    merchantOnboardingStepReject,
    merchantOnboardingStepUploadFile,
    merchantOnboardingReject,
    merchantOnboadingListAll,
    merchantOnboardingStepList,
    merchantOnboardingGetFile,
    merchantOnboardingCancel,
  )
where

import qualified API.Types.Dashboard.AppManagement.MerchantOnboarding
import qualified Data.Aeson
import qualified Domain.Action.Dashboard.AppManagement.MerchantOnboarding
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOnboarding
import qualified Domain.Types.MerchantOnboarding.Handler as DH
import qualified Domain.Types.MerchantOnboardingStep
import qualified "lib-dashboard" Domain.Types.Role as DDashboardRole
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Tools.Auth.DashboardUserAuth

-- | The caller, as these endpoints name them: their person id and the
-- ticket-dashboard role their dashboard role maps onto. Anything outside those
-- four roles has no business here, which is the check rider-dashboard's
-- @getDashboardAccessType@ made.
requestor :: DashboardUser -> Environment.Flow (Kernel.Prelude.Maybe Kernel.Prelude.Text, Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole)
requestor dashboardUser = do
  accessType <- dashboardRequestorAccessType dashboardUser
  role <- case accessType of
    DDashboardRole.TICKET_DASHBOARD_USER -> Kernel.Prelude.pure Domain.Types.MerchantOnboarding.TICKET_DASHBOARD_USER
    DDashboardRole.TICKET_DASHBOARD_MERCHANT -> Kernel.Prelude.pure Domain.Types.MerchantOnboarding.TICKET_DASHBOARD_MERCHANT
    DDashboardRole.TICKET_DASHBOARD_ADMIN -> Kernel.Prelude.pure Domain.Types.MerchantOnboarding.TICKET_DASHBOARD_ADMIN
    DDashboardRole.TICKET_DASHBOARD_APPROVER -> Kernel.Prelude.pure Domain.Types.MerchantOnboarding.TICKET_DASHBOARD_APPROVER
    _ -> throwError $ InvalidRequest "Person does not have proper access"
  Kernel.Prelude.pure (Kernel.Prelude.Just (dashboardRequestorId dashboardUser), Kernel.Prelude.Just role)

-- | The dashboard-database work rider-dashboard did after the application
-- server accepted an approval, which it then stripped from the response.
runDashboardSideHandler :: DH.DashboardSideHandler -> Environment.Flow ()
runDashboardSideHandler handler = case handler.handlerName of
  DH.SET_ROLE_TICKET_DASHBOARD_MERCHANT -> do
    personId <- metadataValue "rid" & fromMaybeM (InternalError "Dashboard Handler failed")
    assignDashboardRoleByAccessType personId DDashboardRole.TICKET_DASHBOARD_MERCHANT
  where
    metadataValue key = snd <$> find (\(k, _) -> k == key) handler.metadata

merchantOnboardingInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Environment.Flow Domain.Types.MerchantOnboarding.MerchantOnboardingAPI)
merchantOnboardingInfo merchantShortId opCity dashboardUser onboardingType' = do
  (requestorId, requestorRole) <- requestor dashboardUser
  Domain.Action.Dashboard.AppManagement.MerchantOnboarding.merchantOnboardingInfo merchantShortId opCity onboardingType' requestorId requestorRole

merchantOnboardingStart :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Environment.Flow Domain.Types.MerchantOnboarding.MerchantOnboardingAPI)
merchantOnboardingStart merchantShortId opCity dashboardUser onboardingType' = do
  (requestorId, requestorRole) <- requestor dashboardUser
  Domain.Action.Dashboard.AppManagement.MerchantOnboarding.merchantOnboardingStart merchantShortId opCity onboardingType' requestorId requestorRole

merchantOnboardingList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Environment.Flow [Domain.Types.MerchantOnboarding.MerchantOnboarding])
merchantOnboardingList merchantShortId opCity dashboardUser = do
  (requestorId, requestorRole) <- requestor dashboardUser
  Domain.Action.Dashboard.AppManagement.MerchantOnboarding.merchantOnboardingList merchantShortId opCity requestorId requestorRole

merchantOnboardingStepSubmit :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Data.Aeson.Value -> Environment.Flow Domain.Types.MerchantOnboarding.MerchantOnboardingAPI)
merchantOnboardingStepSubmit merchantShortId opCity dashboardUser stepId payload = do
  (requestorId, requestorRole) <- requestor dashboardUser
  Domain.Action.Dashboard.AppManagement.MerchantOnboarding.merchantOnboardingStepSubmit merchantShortId opCity stepId requestorId requestorRole payload

merchantOnboardingStepUpdatePayload :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Data.Aeson.Value -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
merchantOnboardingStepUpdatePayload merchantShortId opCity dashboardUser stepId payload = do
  (requestorId, requestorRole) <- requestor dashboardUser
  Domain.Action.Dashboard.AppManagement.MerchantOnboarding.merchantOnboardingStepUpdatePayload merchantShortId opCity stepId requestorId requestorRole payload

-- | The approval also has dashboard-side work to finish, which is why the
-- application server hands a handler back rather than doing it: the role it
-- sets lives in the dashboard database.
merchantOnboardingStepApprove :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Data.Aeson.Value -> Environment.Flow API.Types.Dashboard.AppManagement.MerchantOnboarding.ApproveResponse)
merchantOnboardingStepApprove merchantShortId opCity dashboardUser stepId payload = do
  (requestorId, requestorRole) <- requestor dashboardUser
  resp <- Domain.Action.Dashboard.AppManagement.MerchantOnboarding.merchantOnboardingStepApprove merchantShortId opCity stepId requestorId requestorRole payload
  whenJust resp.handler runDashboardSideHandler
  Kernel.Prelude.pure resp {API.Types.Dashboard.AppManagement.MerchantOnboarding.handler = Kernel.Prelude.Nothing}

merchantOnboardingStepReject :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Data.Aeson.Value -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
merchantOnboardingStepReject merchantShortId opCity dashboardUser stepId payload = do
  (requestorId, requestorRole) <- requestor dashboardUser
  Domain.Action.Dashboard.AppManagement.MerchantOnboarding.merchantOnboardingStepReject merchantShortId opCity stepId requestorId requestorRole payload

merchantOnboardingStepUploadFile :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> API.Types.Dashboard.AppManagement.MerchantOnboarding.UploadFileRequest -> Environment.Flow API.Types.Dashboard.AppManagement.MerchantOnboarding.UploadFileResponse)
merchantOnboardingStepUploadFile merchantShortId opCity dashboardUser stepId payloadKey req = do
  (requestorId, requestorRole) <- requestor dashboardUser
  Domain.Action.Dashboard.AppManagement.MerchantOnboarding.merchantOnboardingStepUploadFile merchantShortId opCity stepId payloadKey requestorId requestorRole req

merchantOnboardingReject :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Data.Aeson.Value -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
merchantOnboardingReject merchantShortId opCity dashboardUser onboardingId req = do
  (requestorId, requestorRole) <- requestor dashboardUser
  Domain.Action.Dashboard.AppManagement.MerchantOnboarding.merchantOnboardingReject merchantShortId opCity onboardingId requestorId requestorRole req

merchantOnboadingListAll :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.OnboardingStatus -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.OnboardingType -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.Flow [Domain.Types.MerchantOnboarding.MerchantOnboardingAPI])
merchantOnboadingListAll merchantShortId opCity dashboardUser mbStatus mbOnboardingType limit offset = do
  (requestorId, requestorRole) <- requestor dashboardUser
  Domain.Action.Dashboard.AppManagement.MerchantOnboarding.merchantOnboadingListAll merchantShortId opCity mbStatus mbOnboardingType limit offset requestorId requestorRole

merchantOnboardingStepList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Environment.Flow [Domain.Types.MerchantOnboardingStep.MerchantOnboardingStep])
merchantOnboardingStepList merchantShortId opCity dashboardUser onboardingId = do
  (requestorId, requestorRole) <- requestor dashboardUser
  Domain.Action.Dashboard.AppManagement.MerchantOnboarding.merchantOnboardingStepList merchantShortId opCity onboardingId requestorId requestorRole

merchantOnboardingGetFile :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Environment.Flow Domain.Types.MerchantOnboarding.GetFileResponse)
merchantOnboardingGetFile merchantShortId opCity dashboardUser onboardingId fileId = do
  (requestorId, requestorRole) <- requestor dashboardUser
  Domain.Action.Dashboard.AppManagement.MerchantOnboarding.merchantOnboardingGetFile merchantShortId opCity onboardingId fileId requestorId requestorRole

merchantOnboardingCancel :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
merchantOnboardingCancel merchantShortId opCity dashboardUser onboardingId = do
  (requestorId, requestorRole) <- requestor dashboardUser
  Domain.Action.Dashboard.AppManagement.MerchantOnboarding.merchantOnboardingCancel merchantShortId opCity onboardingId requestorId requestorRole
