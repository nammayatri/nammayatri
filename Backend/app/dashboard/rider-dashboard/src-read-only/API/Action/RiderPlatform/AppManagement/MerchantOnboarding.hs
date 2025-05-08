{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.AppManagement.MerchantOnboarding
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement
import qualified "rider-app" API.Types.Dashboard.AppManagement.MerchantOnboarding
import qualified Data.Aeson
import qualified Domain.Action.RiderPlatform.AppManagement.MerchantOnboarding
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "rider-app" Domain.Types.MerchantOnboarding
import qualified "rider-app" Domain.Types.MerchantOnboardingStep
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = (MerchantOnboardingInfo :<|> MerchantOnboardingStart :<|> MerchantOnboardingList :<|> MerchantOnboardingStepSubmit :<|> MerchantOnboardingStepUpdatePayload :<|> MerchantOnboardingStepReject :<|> MerchantOnboardingStepApprove :<|> MerchantOnboardingStepUploadFile :<|> MerchantOnboardingReject :<|> MerchantOnboadingListAll :<|> MerchantOnboardingStepList :<|> MerchantOnboardingGetFile :<|> MerchantOnboardingCancel)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = merchantOnboardingInfo merchantId city :<|> merchantOnboardingStart merchantId city :<|> merchantOnboardingList merchantId city :<|> merchantOnboardingStepSubmit merchantId city :<|> merchantOnboardingStepUpdatePayload merchantId city :<|> merchantOnboardingStepReject merchantId city :<|> merchantOnboardingStepApprove merchantId city :<|> merchantOnboardingStepUploadFile merchantId city :<|> merchantOnboardingReject merchantId city :<|> merchantOnboadingListAll merchantId city :<|> merchantOnboardingStepList merchantId city :<|> merchantOnboardingGetFile merchantId city :<|> merchantOnboardingCancel merchantId city

type MerchantOnboardingInfo =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.MERCHANT_ONBOARDING / 'API.Types.Dashboard.AppManagement.MerchantOnboarding.MERCHANT_ONBOARDING_INFO)
      :> API.Types.Dashboard.AppManagement.MerchantOnboarding.MerchantOnboardingInfo
  )

type MerchantOnboardingStart =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.MERCHANT_ONBOARDING / 'API.Types.Dashboard.AppManagement.MerchantOnboarding.MERCHANT_ONBOARDING_START)
      :> API.Types.Dashboard.AppManagement.MerchantOnboarding.MerchantOnboardingStart
  )

type MerchantOnboardingList =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.MERCHANT_ONBOARDING / 'API.Types.Dashboard.AppManagement.MerchantOnboarding.MERCHANT_ONBOARDING_LIST)
      :> API.Types.Dashboard.AppManagement.MerchantOnboarding.MerchantOnboardingList
  )

type MerchantOnboardingStepSubmit =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.MERCHANT_ONBOARDING / 'API.Types.Dashboard.AppManagement.MerchantOnboarding.MERCHANT_ONBOARDING_STEP_SUBMIT)
      :> API.Types.Dashboard.AppManagement.MerchantOnboarding.MerchantOnboardingStepSubmit
  )

type MerchantOnboardingStepUpdatePayload =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.MERCHANT_ONBOARDING / 'API.Types.Dashboard.AppManagement.MerchantOnboarding.MERCHANT_ONBOARDING_STEP_UPDATE_PAYLOAD)
      :> API.Types.Dashboard.AppManagement.MerchantOnboarding.MerchantOnboardingStepUpdatePayload
  )

type MerchantOnboardingStepReject =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.MERCHANT_ONBOARDING / 'API.Types.Dashboard.AppManagement.MerchantOnboarding.MERCHANT_ONBOARDING_STEP_REJECT)
      :> API.Types.Dashboard.AppManagement.MerchantOnboarding.MerchantOnboardingStepReject
  )

type MerchantOnboardingStepApprove =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.MERCHANT_ONBOARDING / 'API.Types.Dashboard.AppManagement.MerchantOnboarding.MERCHANT_ONBOARDING_STEP_APPROVE)
      :> API.Types.Dashboard.AppManagement.MerchantOnboarding.MerchantOnboardingStepApprove
  )

type MerchantOnboardingStepUploadFile =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.MERCHANT_ONBOARDING / 'API.Types.Dashboard.AppManagement.MerchantOnboarding.MERCHANT_ONBOARDING_STEP_UPLOAD_FILE)
      :> API.Types.Dashboard.AppManagement.MerchantOnboarding.MerchantOnboardingStepUploadFile
  )

type MerchantOnboardingReject =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.MERCHANT_ONBOARDING / 'API.Types.Dashboard.AppManagement.MerchantOnboarding.MERCHANT_ONBOARDING_REJECT)
      :> API.Types.Dashboard.AppManagement.MerchantOnboarding.MerchantOnboardingReject
  )

type MerchantOnboadingListAll =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.MERCHANT_ONBOARDING / 'API.Types.Dashboard.AppManagement.MerchantOnboarding.MERCHANT_ONBOADING_LIST_ALL)
      :> API.Types.Dashboard.AppManagement.MerchantOnboarding.MerchantOnboadingListAll
  )

type MerchantOnboardingStepList =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.MERCHANT_ONBOARDING / 'API.Types.Dashboard.AppManagement.MerchantOnboarding.MERCHANT_ONBOARDING_STEP_LIST)
      :> API.Types.Dashboard.AppManagement.MerchantOnboarding.MerchantOnboardingStepList
  )

type MerchantOnboardingGetFile =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.MERCHANT_ONBOARDING / 'API.Types.Dashboard.AppManagement.MerchantOnboarding.MERCHANT_ONBOARDING_GET_FILE)
      :> API.Types.Dashboard.AppManagement.MerchantOnboarding.MerchantOnboardingGetFile
  )

type MerchantOnboardingCancel =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.MERCHANT_ONBOARDING / 'API.Types.Dashboard.AppManagement.MerchantOnboarding.MERCHANT_ONBOARDING_CANCEL)
      :> API.Types.Dashboard.AppManagement.MerchantOnboarding.MerchantOnboardingCancel
  )

merchantOnboardingInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Environment.FlowHandler Domain.Types.MerchantOnboarding.MerchantOnboardingAPI)
merchantOnboardingInfo merchantShortId opCity apiTokenInfo onboardingType requestorId requestorRole = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.MerchantOnboarding.merchantOnboardingInfo merchantShortId opCity apiTokenInfo onboardingType requestorId requestorRole

merchantOnboardingStart :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Environment.FlowHandler Domain.Types.MerchantOnboarding.MerchantOnboardingAPI)
merchantOnboardingStart merchantShortId opCity apiTokenInfo onboardingType requestorId requestorRole = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.MerchantOnboarding.merchantOnboardingStart merchantShortId opCity apiTokenInfo onboardingType requestorId requestorRole

merchantOnboardingList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Environment.FlowHandler [Domain.Types.MerchantOnboarding.MerchantOnboarding])
merchantOnboardingList merchantShortId opCity apiTokenInfo requestorId requestorRole = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.MerchantOnboarding.merchantOnboardingList merchantShortId opCity apiTokenInfo requestorId requestorRole

merchantOnboardingStepSubmit :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Data.Aeson.Value -> Environment.FlowHandler Domain.Types.MerchantOnboarding.MerchantOnboardingAPI)
merchantOnboardingStepSubmit merchantShortId opCity apiTokenInfo stepId requestorId requestorRole req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.MerchantOnboarding.merchantOnboardingStepSubmit merchantShortId opCity apiTokenInfo stepId requestorId requestorRole req

merchantOnboardingStepUpdatePayload :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Data.Aeson.Value -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
merchantOnboardingStepUpdatePayload merchantShortId opCity apiTokenInfo stepId requestorId requestorRole req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.MerchantOnboarding.merchantOnboardingStepUpdatePayload merchantShortId opCity apiTokenInfo stepId requestorId requestorRole req

merchantOnboardingStepReject :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Data.Aeson.Value -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
merchantOnboardingStepReject merchantShortId opCity apiTokenInfo stepId requestorId requestorRole req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.MerchantOnboarding.merchantOnboardingStepReject merchantShortId opCity apiTokenInfo stepId requestorId requestorRole req

merchantOnboardingStepApprove :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Data.Aeson.Value -> Environment.FlowHandler API.Types.Dashboard.AppManagement.MerchantOnboarding.ApproveResponse)
merchantOnboardingStepApprove merchantShortId opCity apiTokenInfo stepId requestorId requestorRole req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.MerchantOnboarding.merchantOnboardingStepApprove merchantShortId opCity apiTokenInfo stepId requestorId requestorRole req

merchantOnboardingStepUploadFile :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> API.Types.Dashboard.AppManagement.MerchantOnboarding.UploadFileRequest -> Environment.FlowHandler API.Types.Dashboard.AppManagement.MerchantOnboarding.UploadFileResponse)
merchantOnboardingStepUploadFile merchantShortId opCity apiTokenInfo stepId payloadKey requestorId requestorRole req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.MerchantOnboarding.merchantOnboardingStepUploadFile merchantShortId opCity apiTokenInfo stepId payloadKey requestorId requestorRole req

merchantOnboardingReject :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Data.Aeson.Value -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
merchantOnboardingReject merchantShortId opCity apiTokenInfo onboardingId requestorId requestorRole req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.MerchantOnboarding.merchantOnboardingReject merchantShortId opCity apiTokenInfo onboardingId requestorId requestorRole req

merchantOnboadingListAll :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.OnboardingStatus -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.OnboardingType -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.FlowHandler [Domain.Types.MerchantOnboarding.MerchantOnboardingAPI])
merchantOnboadingListAll merchantShortId opCity apiTokenInfo requestorId requestorRole status onboardingType limit offset = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.MerchantOnboarding.merchantOnboadingListAll merchantShortId opCity apiTokenInfo requestorId requestorRole status onboardingType limit offset

merchantOnboardingStepList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Environment.FlowHandler [Domain.Types.MerchantOnboardingStep.MerchantOnboardingStep])
merchantOnboardingStepList merchantShortId opCity apiTokenInfo onboardingId requestorId requestorRole = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.MerchantOnboarding.merchantOnboardingStepList merchantShortId opCity apiTokenInfo onboardingId requestorId requestorRole

merchantOnboardingGetFile :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Environment.FlowHandler Domain.Types.MerchantOnboarding.GetFileResponse)
merchantOnboardingGetFile merchantShortId opCity apiTokenInfo onboardingId fileId requestorId requestorRole = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.MerchantOnboarding.merchantOnboardingGetFile merchantShortId opCity apiTokenInfo onboardingId fileId requestorId requestorRole

merchantOnboardingCancel :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
merchantOnboardingCancel merchantShortId opCity apiTokenInfo onboardingId requestorId requestorRole = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.MerchantOnboarding.merchantOnboardingCancel merchantShortId opCity apiTokenInfo onboardingId requestorId requestorRole
