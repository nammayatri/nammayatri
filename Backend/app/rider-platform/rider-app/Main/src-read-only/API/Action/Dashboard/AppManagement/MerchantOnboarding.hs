{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.AppManagement.MerchantOnboarding
  ( API.Types.Dashboard.AppManagement.MerchantOnboarding.API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement.MerchantOnboarding
import qualified Data.Aeson
import qualified Domain.Action.Dashboard.AppManagement.MerchantOnboarding as Domain.Action.Dashboard.AppManagement.MerchantOnboarding
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

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.Dashboard.AppManagement.MerchantOnboarding.API)
handler merchantId city = merchantOnboardingInfo merchantId city :<|> merchantOnboardingStart merchantId city :<|> merchantOnboardingList merchantId city :<|> merchantOnboardingStepSubmit merchantId city :<|> merchantOnboardingStepUpdatePayload merchantId city :<|> merchantOnboardingStepReject merchantId city :<|> merchantOnboardingStepApprove merchantId city :<|> merchantOnboardingStepUploadFile merchantId city :<|> merchantOnboardingReject merchantId city :<|> merchantOnboadingListAll merchantId city :<|> merchantOnboardingStepList merchantId city

merchantOnboardingInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Domain.Types.MerchantOnboarding.OnboardingType -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler Domain.Types.MerchantOnboarding.MerchantOnboardingAPI)
merchantOnboardingInfo a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.MerchantOnboarding.merchantOnboardingInfo a4 a3 a2 a1

merchantOnboardingStart :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Domain.Types.MerchantOnboarding.OnboardingType -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler Domain.Types.MerchantOnboarding.MerchantOnboardingAPI)
merchantOnboardingStart a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.MerchantOnboarding.merchantOnboardingStart a4 a3 a2 a1

merchantOnboardingList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler [Domain.Types.MerchantOnboarding.MerchantOnboarding])
merchantOnboardingList a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.MerchantOnboarding.merchantOnboardingList a3 a2 a1

merchantOnboardingStepSubmit :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Data.Aeson.Value -> Environment.FlowHandler Domain.Types.MerchantOnboarding.MerchantOnboardingAPI)
merchantOnboardingStepSubmit a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.MerchantOnboarding.merchantOnboardingStepSubmit a5 a4 a3 a2 a1

merchantOnboardingStepUpdatePayload :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Data.Aeson.Value -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
merchantOnboardingStepUpdatePayload a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.MerchantOnboarding.merchantOnboardingStepUpdatePayload a5 a4 a3 a2 a1

merchantOnboardingStepReject :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Data.Aeson.Value -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
merchantOnboardingStepReject a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.MerchantOnboarding.merchantOnboardingStepReject a5 a4 a3 a2 a1

merchantOnboardingStepApprove :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Data.Aeson.Value -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
merchantOnboardingStepApprove a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.MerchantOnboarding.merchantOnboardingStepApprove a5 a4 a3 a2 a1

merchantOnboardingStepUploadFile :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> API.Types.Dashboard.AppManagement.MerchantOnboarding.UploadFileRequest -> Environment.FlowHandler API.Types.Dashboard.AppManagement.MerchantOnboarding.UploadFileResponse)
merchantOnboardingStepUploadFile a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.MerchantOnboarding.merchantOnboardingStepUploadFile a6 a5 a4 a3 a2 a1

merchantOnboardingReject :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Data.Aeson.Value -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
merchantOnboardingReject a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.MerchantOnboarding.merchantOnboardingReject a5 a4 a3 a2 a1

merchantOnboadingListAll :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.OnboardingStatus -> Environment.FlowHandler [Domain.Types.MerchantOnboarding.MerchantOnboarding])
merchantOnboadingListAll a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.MerchantOnboarding.merchantOnboadingListAll a4 a3 a2 a1

merchantOnboardingStepList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler [Domain.Types.MerchantOnboardingStep.MerchantOnboardingStep])
merchantOnboardingStepList a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.MerchantOnboarding.merchantOnboardingStepList a5 a4 a3 a2 a1
