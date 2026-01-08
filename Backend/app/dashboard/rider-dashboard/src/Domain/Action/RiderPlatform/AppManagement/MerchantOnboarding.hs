module Domain.Action.RiderPlatform.AppManagement.MerchantOnboarding
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
    getDashboardAccessType,
  )
where

import qualified API.Client.RiderPlatform.AppManagement
import qualified "rider-app" API.Types.Dashboard.AppManagement.Endpoints.MerchantOnboarding as AppMO
import qualified API.Types.Dashboard.AppManagement.MerchantOnboarding
import "rider-app" API.Types.Dashboard.AppManagement.MerchantOnboarding ()
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "rider-app" Domain.Types.MerchantOnboarding
import qualified "rider-app" Domain.Types.MerchantOnboarding as MO
import qualified "rider-app" Domain.Types.MerchantOnboarding.Handler as DH
import qualified Domain.Types.MerchantOnboardingStep
import qualified "lib-dashboard" Domain.Types.Role
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import "lib-dashboard" Storage.Beam.BeamFlow
import Storage.Beam.CommonInstances ()
import qualified "lib-dashboard" Storage.Queries.Person as QP
import qualified "lib-dashboard" Storage.Queries.Role as QR
import Storage.Types (FileType (..))
import Tools.Auth.Api
import Tools.Auth.Merchant
import "lib-dashboard" Tools.Error

getDashboardAccessType :: (BeamFlow m r, EncFlow m r) => Kernel.Prelude.Text -> m Domain.Types.MerchantOnboarding.RequestorRole
getDashboardAccessType personId = do
  person <- QP.findById (Kernel.Types.Id.Id personId) >>= fromMaybeM (InvalidRequest "Person not found")
  role <- QR.findById person.roleId >>= fromMaybeM (InvalidRequest "Role is not assigned for this user")
  let accessType = role.dashboardAccessType
  case accessType of
    Domain.Types.Role.TICKET_DASHBOARD_USER -> pure $ Domain.Types.MerchantOnboarding.TICKET_DASHBOARD_USER
    Domain.Types.Role.TICKET_DASHBOARD_MERCHANT -> pure $ Domain.Types.MerchantOnboarding.TICKET_DASHBOARD_MERCHANT
    Domain.Types.Role.TICKET_DASHBOARD_ADMIN -> pure $ Domain.Types.MerchantOnboarding.TICKET_DASHBOARD_ADMIN
    Domain.Types.Role.TICKET_DASHBOARD_APPROVER -> pure $ Domain.Types.MerchantOnboarding.TICKET_DASHBOARD_APPROVER
    _ -> throwError $ InvalidRequest "Person does not have proper access"

merchantOnboardingInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe MO.RequestorRole -> Environment.Flow Domain.Types.MerchantOnboarding.MerchantOnboardingAPI)
merchantOnboardingInfo merchantShortId opCity apiTokenInfo onboardingType _ _ = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = apiTokenInfo.personId.getId
  requestorRole <- getDashboardAccessType requestorId
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.merchantOnboardingDSL.merchantOnboardingInfo) onboardingType (Just requestorId) (Just requestorRole)

merchantOnboardingStart :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe MO.RequestorRole -> Environment.Flow Domain.Types.MerchantOnboarding.MerchantOnboardingAPI)
merchantOnboardingStart merchantShortId opCity apiTokenInfo onboardingType _ _ = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = apiTokenInfo.personId.getId
  requestorRole <- getDashboardAccessType requestorId
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.merchantOnboardingDSL.merchantOnboardingStart) onboardingType (Just requestorId) (Just requestorRole)

merchantOnboardingList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe MO.RequestorRole -> Environment.Flow [Domain.Types.MerchantOnboarding.MerchantOnboarding])
merchantOnboardingList merchantShortId opCity apiTokenInfo _ _ = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = apiTokenInfo.personId.getId
  requestorRole <- getDashboardAccessType requestorId
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.merchantOnboardingDSL.merchantOnboardingList) (Just requestorId) (Just requestorRole)

merchantOnboardingStepSubmit :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe MO.RequestorRole -> Data.Aeson.Value -> Environment.Flow Domain.Types.MerchantOnboarding.MerchantOnboardingAPI)
merchantOnboardingStepSubmit merchantShortId opCity apiTokenInfo stepId _ _ req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = apiTokenInfo.personId.getId
  requestorRole <- getDashboardAccessType requestorId
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.merchantOnboardingDSL.merchantOnboardingStepSubmit) stepId (Just requestorId) (Just requestorRole) req

merchantOnboardingStepUpdatePayload :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe MO.RequestorRole -> Data.Aeson.Value -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
merchantOnboardingStepUpdatePayload merchantShortId opCity apiTokenInfo stepId _ _ req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = apiTokenInfo.personId.getId
  requestorRole <- getDashboardAccessType requestorId
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.merchantOnboardingDSL.merchantOnboardingStepUpdatePayload) stepId (Just requestorId) (Just requestorRole) req

merchantOnboardingStepReject :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe MO.RequestorRole -> Data.Aeson.Value -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
merchantOnboardingStepReject merchantShortId opCity apiTokenInfo stepId _ _ req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = apiTokenInfo.personId.getId
  requestorRole <- getDashboardAccessType requestorId
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.merchantOnboardingDSL.merchantOnboardingStepReject) stepId (Just requestorId) (Just requestorRole) req

merchantOnboardingStepApprove :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe MO.RequestorRole -> Data.Aeson.Value -> Environment.Flow API.Types.Dashboard.AppManagement.MerchantOnboarding.ApproveResponse)
merchantOnboardingStepApprove merchantShortId opCity apiTokenInfo stepId _ _ req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = apiTokenInfo.personId.getId
  requestorRole <- getDashboardAccessType requestorId
  resp <- API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.merchantOnboardingDSL.merchantOnboardingStepApprove) stepId (Just requestorId) (Just requestorRole) req
  whenJust resp.handler dashboardSideHandler
  return $ resp {AppMO.handler = Nothing}

merchantOnboardingStepUploadFile :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe MO.RequestorRole -> API.Types.Dashboard.AppManagement.MerchantOnboarding.UploadFileRequest -> Environment.Flow API.Types.Dashboard.AppManagement.MerchantOnboarding.UploadFileResponse)
merchantOnboardingStepUploadFile merchantShortId opCity apiTokenInfo stepId payloadKey _ _ req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  logDebug $ "DashboardReceived request with fileType: " <> show req.fileType <> " and reqContentType: " <> show req.reqContentType
  unless (req.fileType `elem` [PDF, Image] && req.reqContentType `elem` ["image/png", "image/jpeg", "application/pdf"]) $
    throwError $ InvalidRequest "Only support PDF/Image file supported"
  let requestorId = apiTokenInfo.personId.getId
  requestorRole <- getDashboardAccessType requestorId
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (addMultipartBoundary "XXX00XXX" . (.merchantOnboardingDSL.merchantOnboardingStepUploadFile)) stepId payloadKey (Just requestorId) (Just requestorRole) req
  where
    addMultipartBoundary :: LBS.ByteString -> (Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe MO.RequestorRole -> (LBS.ByteString, req) -> res) -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe MO.RequestorRole -> req -> res
    addMultipartBoundary boundary clientFn stepId_ payloadKey_ requestorId_ requestorRole_ reqBody = clientFn stepId_ payloadKey_ requestorId_ requestorRole_ (boundary, reqBody)

merchantOnboardingReject :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe MO.RequestorRole -> Data.Aeson.Value -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
merchantOnboardingReject merchantShortId opCity apiTokenInfo onboardingId _ _ req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = apiTokenInfo.personId.getId
  requestorRole <- getDashboardAccessType requestorId
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.merchantOnboardingDSL.merchantOnboardingReject) onboardingId (Just requestorId) (Just requestorRole) req

merchantOnboadingListAll :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe MO.RequestorRole -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.OnboardingStatus) -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.OnboardingType -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.Flow [Domain.Types.MerchantOnboarding.MerchantOnboardingAPI])
merchantOnboadingListAll merchantShortId opCity apiTokenInfo _ _ status onboardingType limit offset = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = apiTokenInfo.personId.getId
  requestorRole <- getDashboardAccessType requestorId
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.merchantOnboardingDSL.merchantOnboadingListAll) (Just requestorId) (Just requestorRole) status onboardingType limit offset

merchantOnboardingStepList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe MO.RequestorRole -> Environment.Flow [Domain.Types.MerchantOnboardingStep.MerchantOnboardingStep])
merchantOnboardingStepList merchantShortId opCity apiTokenInfo onboardingId _ _ = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = apiTokenInfo.personId.getId
  requestorRole <- getDashboardAccessType requestorId
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.merchantOnboardingDSL.merchantOnboardingStepList) onboardingId (Just requestorId) (Just requestorRole)

merchantOnboardingGetFile :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> Environment.Flow Domain.Types.MerchantOnboarding.GetFileResponse)
merchantOnboardingGetFile merchantShortId opCity apiTokenInfo onboardingId fileId _ _ = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = apiTokenInfo.personId.getId
  requestorRole <- getDashboardAccessType requestorId
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.merchantOnboardingDSL.merchantOnboardingGetFile) onboardingId fileId (Just requestorId) (Just requestorRole)

dashboardSideHandler :: DH.DashboardSideHandler -> Environment.Flow ()
dashboardSideHandler handler = case handler.handlerName of
  DH.SET_ROLE_TICKET_DASHBOARD_MERCHANT -> do
    personId <- getMetadataValue "rid" & fromMaybeM (InternalError "Dashboard Handler failed")
    person <- QP.findById (Kernel.Types.Id.Id personId) >>= fromMaybeM (InvalidRequest "Person not found")
    role <- QR.findByDashboardAccessType Domain.Types.Role.TICKET_DASHBOARD_MERCHANT >>= fromMaybeM (RoleDoesNotExist (show Domain.Types.Role.TICKET_DASHBOARD_MERCHANT))
    QP.updatePersonRole person.id role
  where
    getMetadataValue :: Text -> Maybe Text
    getMetadataValue key = snd <$> find (\(k, _) -> k == key) handler.metadata

merchantOnboardingCancel :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
merchantOnboardingCancel merchantShortId opCity apiTokenInfo onboardingId _requestorId _requestorRole = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = apiTokenInfo.personId.getId
  requestorRole <- getDashboardAccessType requestorId
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.merchantOnboardingDSL.merchantOnboardingCancel) onboardingId (Just requestorId) (Just requestorRole)
