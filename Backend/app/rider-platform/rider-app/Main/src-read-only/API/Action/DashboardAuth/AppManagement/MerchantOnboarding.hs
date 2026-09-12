{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.AppManagement.MerchantOnboarding
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement.MerchantOnboarding
import qualified Data.Aeson
import qualified Domain.Action.Dashboard.AppManagement.MerchantOnboarding
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.MerchantOnboarding
import qualified "this" Domain.Types.MerchantOnboardingStep
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = (MerchantOnboardingInfo :<|> MerchantOnboardingStart :<|> MerchantOnboardingList :<|> MerchantOnboardingStepSubmit :<|> MerchantOnboardingStepUpdatePayload :<|> MerchantOnboardingStepReject :<|> MerchantOnboardingStepApprove :<|> MerchantOnboardingStepUploadFile :<|> MerchantOnboardingReject :<|> MerchantOnboadingListAll :<|> MerchantOnboardingStepList :<|> MerchantOnboardingGetFile :<|> MerchantOnboardingCancel)

type MerchantOnboardingInfo =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_INFO"
      :> API.Types.Dashboard.AppManagement.MerchantOnboarding.MerchantOnboardingInfo
  )

type MerchantOnboardingStart =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_START"
      :> API.Types.Dashboard.AppManagement.MerchantOnboarding.MerchantOnboardingStart
  )

type MerchantOnboardingList =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_LIST"
      :> API.Types.Dashboard.AppManagement.MerchantOnboarding.MerchantOnboardingList
  )

type MerchantOnboardingStepSubmit =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_STEP_SUBMIT"
      :> API.Types.Dashboard.AppManagement.MerchantOnboarding.MerchantOnboardingStepSubmit
  )

type MerchantOnboardingStepUpdatePayload =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_STEP_UPDATE_PAYLOAD"
      :> API.Types.Dashboard.AppManagement.MerchantOnboarding.MerchantOnboardingStepUpdatePayload
  )

type MerchantOnboardingStepReject =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_STEP_REJECT"
      :> API.Types.Dashboard.AppManagement.MerchantOnboarding.MerchantOnboardingStepReject
  )

type MerchantOnboardingStepApprove =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_STEP_APPROVE"
      :> API.Types.Dashboard.AppManagement.MerchantOnboarding.MerchantOnboardingStepApprove
  )

type MerchantOnboardingStepUploadFile =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_STEP_UPLOAD_FILE"
      :> API.Types.Dashboard.AppManagement.MerchantOnboarding.MerchantOnboardingStepUploadFile
  )

type MerchantOnboardingReject =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_REJECT"
      :> API.Types.Dashboard.AppManagement.MerchantOnboarding.MerchantOnboardingReject
  )

type MerchantOnboadingListAll =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOADING_LIST_ALL"
      :> API.Types.Dashboard.AppManagement.MerchantOnboarding.MerchantOnboadingListAll
  )

type MerchantOnboardingStepList =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_STEP_LIST"
      :> API.Types.Dashboard.AppManagement.MerchantOnboarding.MerchantOnboardingStepList
  )

type MerchantOnboardingGetFile =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_GET_FILE"
      :> API.Types.Dashboard.AppManagement.MerchantOnboarding.MerchantOnboardingGetFile
  )

type MerchantOnboardingCancel =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_CANCEL"
      :> API.Types.Dashboard.AppManagement.MerchantOnboarding.MerchantOnboardingCancel
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = merchantOnboardingInfo merchantId city :<|> merchantOnboardingStart merchantId city :<|> merchantOnboardingList merchantId city :<|> merchantOnboardingStepSubmit merchantId city :<|> merchantOnboardingStepUpdatePayload merchantId city :<|> merchantOnboardingStepReject merchantId city :<|> merchantOnboardingStepApprove merchantId city :<|> merchantOnboardingStepUploadFile merchantId city :<|> merchantOnboardingReject merchantId city :<|> merchantOnboadingListAll merchantId city :<|> merchantOnboardingStepList merchantId city :<|> merchantOnboardingGetFile merchantId city :<|> merchantOnboardingCancel merchantId city

merchantOnboardingInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> Environment.FlowHandler Domain.Types.MerchantOnboarding.MerchantOnboardingAPI)
merchantOnboardingInfo a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.MerchantOnboarding.merchantOnboardingInfo a6 a5 a3 a2 a1

merchantOnboardingStart :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> Environment.FlowHandler Domain.Types.MerchantOnboarding.MerchantOnboardingAPI)
merchantOnboardingStart a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.MerchantOnboarding.merchantOnboardingStart a6 a5 a3 a2 a1

merchantOnboardingList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> Environment.FlowHandler [Domain.Types.MerchantOnboarding.MerchantOnboarding])
merchantOnboardingList a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.MerchantOnboarding.merchantOnboardingList a5 a4 a2 a1

merchantOnboardingStepSubmit :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> Data.Aeson.Value -> Environment.FlowHandler Domain.Types.MerchantOnboarding.MerchantOnboardingAPI)
merchantOnboardingStepSubmit a7 a6 _a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.MerchantOnboarding.merchantOnboardingStepSubmit a7 a6 a4 a3 a2 a1

merchantOnboardingStepUpdatePayload :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> Data.Aeson.Value -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
merchantOnboardingStepUpdatePayload a7 a6 _a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.MerchantOnboarding.merchantOnboardingStepUpdatePayload a7 a6 a4 a3 a2 a1

merchantOnboardingStepReject :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> Data.Aeson.Value -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
merchantOnboardingStepReject a7 a6 _a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.MerchantOnboarding.merchantOnboardingStepReject a7 a6 a4 a3 a2 a1

merchantOnboardingStepApprove :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> Data.Aeson.Value -> Environment.FlowHandler API.Types.Dashboard.AppManagement.MerchantOnboarding.ApproveResponse)
merchantOnboardingStepApprove a7 a6 _a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.MerchantOnboarding.merchantOnboardingStepApprove a7 a6 a4 a3 a2 a1

merchantOnboardingStepUploadFile :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> API.Types.Dashboard.AppManagement.MerchantOnboarding.UploadFileRequest -> Environment.FlowHandler API.Types.Dashboard.AppManagement.MerchantOnboarding.UploadFileResponse)
merchantOnboardingStepUploadFile a8 a7 _a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.MerchantOnboarding.merchantOnboardingStepUploadFile a8 a7 a5 a4 a3 a2 a1

merchantOnboardingReject :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> Data.Aeson.Value -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
merchantOnboardingReject a7 a6 _a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.MerchantOnboarding.merchantOnboardingReject a7 a6 a4 a3 a2 a1

merchantOnboadingListAll :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.OnboardingStatus) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.OnboardingType) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler [Domain.Types.MerchantOnboarding.MerchantOnboardingAPI])
merchantOnboadingListAll a9 a8 _a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.MerchantOnboarding.merchantOnboadingListAll a9 a8 a6 a5 a4 a3 a2 a1

merchantOnboardingStepList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> Environment.FlowHandler [Domain.Types.MerchantOnboardingStep.MerchantOnboardingStep])
merchantOnboardingStepList a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.MerchantOnboarding.merchantOnboardingStepList a6 a5 a3 a2 a1

merchantOnboardingGetFile :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> Environment.FlowHandler Domain.Types.MerchantOnboarding.GetFileResponse)
merchantOnboardingGetFile a7 a6 _a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.MerchantOnboarding.merchantOnboardingGetFile a7 a6 a4 a3 a2 a1

merchantOnboardingCancel :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.MerchantOnboarding.RequestorRole) -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
merchantOnboardingCancel a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.MerchantOnboarding.merchantOnboardingCancel a6 a5 a3 a2 a1
