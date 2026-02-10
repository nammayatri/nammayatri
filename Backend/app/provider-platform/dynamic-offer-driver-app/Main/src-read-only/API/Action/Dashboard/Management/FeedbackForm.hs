{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.FeedbackForm
  ( API.Types.ProviderPlatform.Management.FeedbackForm.API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.FeedbackForm
import qualified Domain.Action.Dashboard.Management.FeedbackForm
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Management.FeedbackForm.API)
handler merchantId city = postFeedbackFormCreate merchantId city :<|> putFeedbackFormUpdate merchantId city :<|> deleteFeedbackFormDelete merchantId city :<|> getFeedbackForm merchantId city

postFeedbackFormCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.FeedbackForm.CreateFeedbackFormReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FeedbackForm.CreateFeedbackFormRes)
postFeedbackFormCreate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.FeedbackForm.postFeedbackFormCreate a3 a2 a1

putFeedbackFormUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.FeedbackForm.UpdateFeedbackFormReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putFeedbackFormUpdate a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.FeedbackForm.putFeedbackFormUpdate a4 a3 a2 a1

deleteFeedbackFormDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteFeedbackFormDelete a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.FeedbackForm.deleteFeedbackFormDelete a3 a2 a1

getFeedbackForm :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FeedbackForm.FeedbackFormRes)
getFeedbackForm a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.FeedbackForm.getFeedbackForm a3 a2 a1
