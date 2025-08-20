{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.SearchSuspect
  ( API,
    handler,
  )
where

import qualified API.Types.UI.SearchSuspect
import qualified Data.Text
import qualified Domain.Action.UI.SearchSuspect
import qualified Domain.Types.Suspect
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import Kernel.Utils.Common
import Servant
import "lib-dashboard" Tools.Auth
import Tools.Auth.Webhook

type API =
  ( DashboardAuth 'DASHBOARD_USER :> "search" :> "suspectList" :> ReqBody '[JSON] API.Types.UI.SearchSuspect.SearchSuspectReqList
      :> Post
           '[JSON]
           API.Types.UI.SearchSuspect.SuspectsList
      :<|> DashboardAuth 'DASHBOARD_ADMIN
      :> "check"
      :> "suspectStatusHistory"
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> ReqBody
           '[JSON]
           API.Types.UI.SearchSuspect.SearchSuspectReq
      :> Post
           '[JSON]
           API.Types.UI.SearchSuspect.CheckSuspectStatusHistoryResp
      :<|> DashboardAuth 'MERCHANT_MAKER
      :> "merchant"
      :> "check"
      :> "suspectStatusHistory"
      :> ReqBody
           '[JSON]
           API.Types.UI.SearchSuspect.SearchSuspectReq
      :> Post
           '[JSON]
           API.Types.UI.SearchSuspect.CheckSuspectStatusHistoryResp
      :<|> SafetyWebhookAuth 'MERCHANT_SERVER
      :> "partner"
      :> "search"
      :> "agent"
      :> ReqBody
           '[JSON]
           API.Types.UI.SearchSuspect.SearchSuspectReqList
      :> Post
           '[JSON]
           API.Types.UI.SearchSuspect.SuspectsList
      :<|> DashboardAuth 'DASHBOARD_ADMIN
      :> "suspect"
      :> "list"
      :> QueryParam
           "dl"
           Data.Text.Text
      :> QueryParam
           "flaggedCategory"
           Data.Text.Text
      :> QueryParam
           "flaggedStatus"
           Domain.Types.Suspect.FlaggedStatus
      :> QueryParam
           "from"
           Kernel.Prelude.UTCTime
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam
           "partnerName"
           Data.Text.Text
      :> QueryParam
           "to"
           Kernel.Prelude.UTCTime
      :> QueryParam
           "voterId"
           Data.Text.Text
      :> Get
           '[JSON]
           API.Types.UI.SearchSuspect.SuspectsList
      :<|> DashboardAuth 'MERCHANT_ADMIN
      :> "partner"
      :> "suspect"
      :> "list"
      :> QueryParam
           "from"
           Kernel.Prelude.UTCTime
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam
           "to"
           Kernel.Prelude.UTCTime
      :> Get
           '[JSON]
           API.Types.UI.SearchSuspect.SuspectsList
  )

handler :: Environment.FlowServer API
handler = postSearchSuspectList :<|> postCheckSuspectStatusHistory :<|> postMerchantCheckSuspectStatusHistory :<|> postPartnerSearchAgent :<|> getSuspectList :<|> getPartnerSuspectList

postSearchSuspectList :: (TokenInfo -> API.Types.UI.SearchSuspect.SearchSuspectReqList -> Environment.FlowHandler API.Types.UI.SearchSuspect.SuspectsList)
postSearchSuspectList a2 a1 = withFlowHandlerAPI' $ Domain.Action.UI.SearchSuspect.postSearchSuspectList a2 a1

postCheckSuspectStatusHistory :: (TokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> API.Types.UI.SearchSuspect.SearchSuspectReq -> Environment.FlowHandler API.Types.UI.SearchSuspect.CheckSuspectStatusHistoryResp)
postCheckSuspectStatusHistory a4 a3 a2 a1 = withFlowHandlerAPI' $ Domain.Action.UI.SearchSuspect.postCheckSuspectStatusHistory a4 a3 a2 a1

postMerchantCheckSuspectStatusHistory :: (TokenInfo -> API.Types.UI.SearchSuspect.SearchSuspectReq -> Environment.FlowHandler API.Types.UI.SearchSuspect.CheckSuspectStatusHistoryResp)
postMerchantCheckSuspectStatusHistory a2 a1 = withFlowHandlerAPI' $ Domain.Action.UI.SearchSuspect.postMerchantCheckSuspectStatusHistory a2 a1

postPartnerSearchAgent :: (AuthToken -> API.Types.UI.SearchSuspect.SearchSuspectReqList -> Environment.FlowHandler API.Types.UI.SearchSuspect.SuspectsList)
postPartnerSearchAgent a2 a1 = withFlowHandlerAPI' $ Domain.Action.UI.SearchSuspect.postPartnerSearchAgent a2 a1

getSuspectList :: (TokenInfo -> Kernel.Prelude.Maybe Data.Text.Text -> Kernel.Prelude.Maybe Data.Text.Text -> Kernel.Prelude.Maybe Domain.Types.Suspect.FlaggedStatus -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Data.Text.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Data.Text.Text -> Environment.FlowHandler API.Types.UI.SearchSuspect.SuspectsList)
getSuspectList a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withFlowHandlerAPI' $ Domain.Action.UI.SearchSuspect.getSuspectList a10 a9 a8 a7 a6 a5 a4 a3 a2 a1

getPartnerSuspectList :: (TokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.FlowHandler API.Types.UI.SearchSuspect.SuspectsList)
getPartnerSuspectList a5 a4 a3 a2 a1 = withFlowHandlerAPI' $ Domain.Action.UI.SearchSuspect.getPartnerSuspectList a5 a4 a3 a2 a1
