{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Management.FeedbackForm
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.FeedbackForm
import qualified Domain.Action.ProviderPlatform.Management.FeedbackForm
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("feedbackForm" :> (PostFeedbackFormCreate :<|> PutFeedbackFormUpdate :<|> DeleteFeedbackFormDelete :<|> GetFeedbackForm))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postFeedbackFormCreate merchantId city :<|> putFeedbackFormUpdate merchantId city :<|> deleteFeedbackFormDelete merchantId city :<|> getFeedbackForm merchantId city

type PostFeedbackFormCreate =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.FEEDBACK_FORM) / ('API.Types.ProviderPlatform.Management.FeedbackForm.POST_FEEDBACK_FORM_CREATE))
      :> API.Types.ProviderPlatform.Management.FeedbackForm.PostFeedbackFormCreate
  )

type PutFeedbackFormUpdate =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.FEEDBACK_FORM) / ('API.Types.ProviderPlatform.Management.FeedbackForm.PUT_FEEDBACK_FORM_UPDATE))
      :> API.Types.ProviderPlatform.Management.FeedbackForm.PutFeedbackFormUpdate
  )

type DeleteFeedbackFormDelete =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.FEEDBACK_FORM) / ('API.Types.ProviderPlatform.Management.FeedbackForm.DELETE_FEEDBACK_FORM_DELETE))
      :> API.Types.ProviderPlatform.Management.FeedbackForm.DeleteFeedbackFormDelete
  )

type GetFeedbackForm =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.FEEDBACK_FORM) / ('API.Types.ProviderPlatform.Management.FeedbackForm.GET_FEEDBACK_FORM))
      :> API.Types.ProviderPlatform.Management.FeedbackForm.GetFeedbackForm
  )

postFeedbackFormCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.FeedbackForm.CreateFeedbackFormReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FeedbackForm.CreateFeedbackFormRes)
postFeedbackFormCreate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.FeedbackForm.postFeedbackFormCreate merchantShortId opCity apiTokenInfo req

putFeedbackFormUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.FeedbackForm.UpdateFeedbackFormReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putFeedbackFormUpdate merchantShortId opCity apiTokenInfo feedbackFormId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.FeedbackForm.putFeedbackFormUpdate merchantShortId opCity apiTokenInfo feedbackFormId req

deleteFeedbackFormDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteFeedbackFormDelete merchantShortId opCity apiTokenInfo feedbackFormId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.FeedbackForm.deleteFeedbackFormDelete merchantShortId opCity apiTokenInfo feedbackFormId

getFeedbackForm :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FeedbackForm.FeedbackFormRes)
getFeedbackForm merchantShortId opCity apiTokenInfo feedbackFormId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.FeedbackForm.getFeedbackForm merchantShortId opCity apiTokenInfo feedbackFormId
