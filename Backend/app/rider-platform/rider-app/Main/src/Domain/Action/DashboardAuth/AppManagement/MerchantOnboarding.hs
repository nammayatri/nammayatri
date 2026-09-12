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

merchantOnboardingInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Environment.Flow Domain.Types.MerchantOnboarding.MerchantOnboardingAPI)
merchantOnboardingInfo a6 a5 a4 a3 _a2 _a1 = do
  (requestorId, requestorRole) <- requestor a4
  Domain.Action.Dashboard.AppManagement.MerchantOnboarding.merchantOnboardingInfo a6 a5 a3 requestorId requestorRole

merchantOnboardingStart :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Environment.Flow Domain.Types.MerchantOnboarding.MerchantOnboardingAPI)
merchantOnboardingStart a6 a5 a4 a3 _a2 _a1 = do
  (requestorId, requestorRole) <- requestor a4
  Domain.Action.Dashboard.AppManagement.MerchantOnboarding.merchantOnboardingStart a6 a5 a3 requestorId requestorRole

merchantOnboardingList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Environment.Flow [Domain.Types.MerchantOnboarding.MerchantOnboarding])
merchantOnboardingList a5 a4 a3 _a2 _a1 = do
  (requestorId, requestorRole) <- requestor a3
  Domain.Action.Dashboard.AppManagement.MerchantOnboarding.merchantOnboardingList a5 a4 requestorId requestorRole

merchantOnboardingStepSubmit :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Data.Aeson.Value -> Environment.Flow Domain.Types.MerchantOnboarding.MerchantOnboardingAPI)
merchantOnboardingStepSubmit a7 a6 a5 a4 _a3 _a2 a1 = do
  (requestorId, requestorRole) <- requestor a5
  Domain.Action.Dashboard.AppManagement.MerchantOnboarding.merchantOnboardingStepSubmit a7 a6 a4 requestorId requestorRole a1

merchantOnboardingStepUpdatePayload :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Data.Aeson.Value -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
merchantOnboardingStepUpdatePayload a7 a6 a5 a4 _a3 _a2 a1 = do
  (requestorId, requestorRole) <- requestor a5
  Domain.Action.Dashboard.AppManagement.MerchantOnboarding.merchantOnboardingStepUpdatePayload a7 a6 a4 requestorId requestorRole a1

-- | The approval also has dashboard-side work to finish, which is why the
-- application server hands a handler back rather than doing it: the role it
-- sets lives in the dashboard database.
merchantOnboardingStepApprove :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Data.Aeson.Value -> Environment.Flow API.Types.Dashboard.AppManagement.MerchantOnboarding.ApproveResponse)
merchantOnboardingStepApprove a7 a6 a5 a4 _a3 _a2 a1 = do
  (requestorId, requestorRole) <- requestor a5
  resp <- Domain.Action.Dashboard.AppManagement.MerchantOnboarding.merchantOnboardingStepApprove a7 a6 a4 requestorId requestorRole a1
  whenJust resp.handler runDashboardSideHandler
  Kernel.Prelude.pure resp {API.Types.Dashboard.AppManagement.MerchantOnboarding.handler = Kernel.Prelude.Nothing}

merchantOnboardingStepReject :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Data.Aeson.Value -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
merchantOnboardingStepReject a7 a6 a5 a4 _a3 _a2 a1 = do
  (requestorId, requestorRole) <- requestor a5
  Domain.Action.Dashboard.AppManagement.MerchantOnboarding.merchantOnboardingStepReject a7 a6 a4 requestorId requestorRole a1

merchantOnboardingStepUploadFile :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> API.Types.Dashboard.AppManagement.MerchantOnboarding.UploadFileRequest -> Environment.Flow API.Types.Dashboard.AppManagement.MerchantOnboarding.UploadFileResponse)
merchantOnboardingStepUploadFile a8 a7 a6 a5 a4 _a3 _a2 a1 = do
  (requestorId, requestorRole) <- requestor a6
  Domain.Action.Dashboard.AppManagement.MerchantOnboarding.merchantOnboardingStepUploadFile a8 a7 a5 a4 requestorId requestorRole a1

merchantOnboardingReject :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Data.Aeson.Value -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
merchantOnboardingReject a7 a6 a5 a4 _a3 _a2 a1 = do
  (requestorId, requestorRole) <- requestor a5
  Domain.Action.Dashboard.AppManagement.MerchantOnboarding.merchantOnboardingReject a7 a6 a4 requestorId requestorRole a1

merchantOnboadingListAll :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.OnboardingStatus -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.OnboardingType -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.Flow [Domain.Types.MerchantOnboarding.MerchantOnboardingAPI])
merchantOnboadingListAll a9 a8 a7 _a6 _a5 a4 a3 a2 a1 = do
  (requestorId, requestorRole) <- requestor a7
  Domain.Action.Dashboard.AppManagement.MerchantOnboarding.merchantOnboadingListAll a9 a8 requestorId requestorRole a4 a3 a2 a1

merchantOnboardingStepList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Environment.Flow [Domain.Types.MerchantOnboardingStep.MerchantOnboardingStep])
merchantOnboardingStepList a6 a5 a4 a3 _a2 _a1 = do
  (requestorId, requestorRole) <- requestor a4
  Domain.Action.Dashboard.AppManagement.MerchantOnboarding.merchantOnboardingStepList a6 a5 a3 requestorId requestorRole

merchantOnboardingGetFile :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Environment.Flow Domain.Types.MerchantOnboarding.GetFileResponse)
merchantOnboardingGetFile a7 a6 a5 a4 a3 _a2 _a1 = do
  (requestorId, requestorRole) <- requestor a5
  Domain.Action.Dashboard.AppManagement.MerchantOnboarding.merchantOnboardingGetFile a7 a6 a4 a3 requestorId requestorRole

merchantOnboardingCancel :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
merchantOnboardingCancel a6 a5 a4 a3 _a2 _a1 = do
  (requestorId, requestorRole) <- requestor a4
  Domain.Action.Dashboard.AppManagement.MerchantOnboarding.merchantOnboardingCancel a6 a5 a3 requestorId requestorRole
