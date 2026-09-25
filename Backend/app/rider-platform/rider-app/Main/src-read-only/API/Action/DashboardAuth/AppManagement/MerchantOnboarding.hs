{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.AppManagement.MerchantOnboarding
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement.MerchantOnboarding
import qualified Data.Aeson
import qualified Domain.Action.DashboardAuth.AppManagement.MerchantOnboarding
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
import qualified Tools.ActorInfo
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = (MerchantOnboardingInfo :<|> MerchantOnboardingStart :<|> MerchantOnboardingList :<|> MerchantOnboardingStepSubmit :<|> MerchantOnboardingStepUpdatePayload :<|> MerchantOnboardingStepReject :<|> MerchantOnboardingStepApprove :<|> MerchantOnboardingStepUploadFile :<|> MerchantOnboardingReject :<|> MerchantOnboadingListAll :<|> MerchantOnboardingStepList :<|> MerchantOnboardingGetFile :<|> MerchantOnboardingCancel)

type MerchantOnboardingInfo =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_INFO"
      :> API.Types.Dashboard.AppManagement.MerchantOnboarding.MerchantOnboardingInfo
  )

type MerchantOnboardingStart =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_START"
      :> API.Types.Dashboard.AppManagement.MerchantOnboarding.MerchantOnboardingStart
  )

type MerchantOnboardingList =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_LIST"
      :> API.Types.Dashboard.AppManagement.MerchantOnboarding.MerchantOnboardingList
  )

type MerchantOnboardingStepSubmit =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_STEP_SUBMIT"
      :> API.Types.Dashboard.AppManagement.MerchantOnboarding.MerchantOnboardingStepSubmit
  )

type MerchantOnboardingStepUpdatePayload =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_STEP_UPDATE_PAYLOAD"
      :> API.Types.Dashboard.AppManagement.MerchantOnboarding.MerchantOnboardingStepUpdatePayload
  )

type MerchantOnboardingStepReject =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_STEP_REJECT"
      :> API.Types.Dashboard.AppManagement.MerchantOnboarding.MerchantOnboardingStepReject
  )

type MerchantOnboardingStepApprove =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_STEP_APPROVE"
      :> API.Types.Dashboard.AppManagement.MerchantOnboarding.MerchantOnboardingStepApprove
  )

type MerchantOnboardingStepUploadFile =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_STEP_UPLOAD_FILE"
      :> API.Types.Dashboard.AppManagement.MerchantOnboarding.MerchantOnboardingStepUploadFile
  )

type MerchantOnboardingReject =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_REJECT"
      :> API.Types.Dashboard.AppManagement.MerchantOnboarding.MerchantOnboardingReject
  )

type MerchantOnboadingListAll =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOADING_LIST_ALL"
      :> API.Types.Dashboard.AppManagement.MerchantOnboarding.MerchantOnboadingListAll
  )

type MerchantOnboardingStepList =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_STEP_LIST"
      :> API.Types.Dashboard.AppManagement.MerchantOnboarding.MerchantOnboardingStepList
  )

type MerchantOnboardingGetFile =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_GET_FILE"
      :> API.Types.Dashboard.AppManagement.MerchantOnboarding.MerchantOnboardingGetFile
  )

type MerchantOnboardingCancel =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_CANCEL"
      :> API.Types.Dashboard.AppManagement.MerchantOnboarding.MerchantOnboardingCancel
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = merchantOnboardingInfo merchantId city :<|> merchantOnboardingStart merchantId city :<|> merchantOnboardingList merchantId city :<|> merchantOnboardingStepSubmit merchantId city :<|> merchantOnboardingStepUpdatePayload merchantId city :<|> merchantOnboardingStepReject merchantId city :<|> merchantOnboardingStepApprove merchantId city :<|> merchantOnboardingStepUploadFile merchantId city :<|> merchantOnboardingReject merchantId city :<|> merchantOnboadingListAll merchantId city :<|> merchantOnboardingStepList merchantId city :<|> merchantOnboardingGetFile merchantId city :<|> merchantOnboardingCancel merchantId city

merchantOnboardingInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Environment.FlowHandler Domain.Types.MerchantOnboarding.MerchantOnboardingAPI)
merchantOnboardingInfo a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.DashboardAuth.AppManagement.MerchantOnboarding.merchantOnboardingInfo a4 a3 a2 a1

merchantOnboardingStart :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Environment.FlowHandler Domain.Types.MerchantOnboarding.MerchantOnboardingAPI)
merchantOnboardingStart a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.DashboardAuth.AppManagement.MerchantOnboarding.merchantOnboardingStart a4 a3 a2 a1

merchantOnboardingList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Environment.FlowHandler [Domain.Types.MerchantOnboarding.MerchantOnboarding])
merchantOnboardingList a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a1 $ Domain.Action.DashboardAuth.AppManagement.MerchantOnboarding.merchantOnboardingList a3 a2 a1

merchantOnboardingStepSubmit :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Data.Aeson.Value -> Environment.FlowHandler Domain.Types.MerchantOnboarding.MerchantOnboardingAPI)
merchantOnboardingStepSubmit a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_STEP_SUBMIT" a3 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.DashboardAuth.AppManagement.MerchantOnboarding.merchantOnboardingStepSubmit a5 a4 a3 a2 a1
    )

merchantOnboardingStepUpdatePayload :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Data.Aeson.Value -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
merchantOnboardingStepUpdatePayload a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_STEP_UPDATE_PAYLOAD" a3 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.DashboardAuth.AppManagement.MerchantOnboarding.merchantOnboardingStepUpdatePayload a5 a4 a3 a2 a1
    )

merchantOnboardingStepReject :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Data.Aeson.Value -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
merchantOnboardingStepReject a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_STEP_REJECT" a3 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.DashboardAuth.AppManagement.MerchantOnboarding.merchantOnboardingStepReject a5 a4 a3 a2 a1
    )

merchantOnboardingStepApprove :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Data.Aeson.Value -> Environment.FlowHandler API.Types.Dashboard.AppManagement.MerchantOnboarding.ApproveResponse)
merchantOnboardingStepApprove a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_STEP_APPROVE" a3 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.DashboardAuth.AppManagement.MerchantOnboarding.merchantOnboardingStepApprove a5 a4 a3 a2 a1
    )

merchantOnboardingStepUploadFile :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> API.Types.Dashboard.AppManagement.MerchantOnboarding.UploadFileRequest -> Environment.FlowHandler API.Types.Dashboard.AppManagement.MerchantOnboarding.UploadFileResponse)
merchantOnboardingStepUploadFile a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_STEP_UPLOAD_FILE" a4 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.DashboardAuth.AppManagement.MerchantOnboarding.merchantOnboardingStepUploadFile a6 a5 a4 a3 a2 a1
    )

merchantOnboardingReject :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Data.Aeson.Value -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
merchantOnboardingReject a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_REJECT" a3 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.DashboardAuth.AppManagement.MerchantOnboarding.merchantOnboardingReject a5 a4 a3 a2 a1
    )

merchantOnboadingListAll :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.OnboardingStatus -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.OnboardingType -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.FlowHandler [Domain.Types.MerchantOnboarding.MerchantOnboardingAPI])
merchantOnboadingListAll a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a5 $ Domain.Action.DashboardAuth.AppManagement.MerchantOnboarding.merchantOnboadingListAll a7 a6 a5 a4 a3 a2 a1

merchantOnboardingStepList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Environment.FlowHandler [Domain.Types.MerchantOnboardingStep.MerchantOnboardingStep])
merchantOnboardingStepList a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.DashboardAuth.AppManagement.MerchantOnboarding.merchantOnboardingStepList a4 a3 a2 a1

merchantOnboardingGetFile :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Environment.FlowHandler Domain.Types.MerchantOnboarding.GetFileResponse)
merchantOnboardingGetFile a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.DashboardAuth.AppManagement.MerchantOnboarding.merchantOnboardingGetFile a5 a4 a3 a2 a1

merchantOnboardingCancel :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
merchantOnboardingCancel a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.DashboardAuth.AppManagement.MerchantOnboarding.merchantOnboardingCancel a4 a3 a2 a1
