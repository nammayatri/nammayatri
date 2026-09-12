{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.IssueManagement
  ( API,
    handler,
  )
where

import qualified API.Action.DashboardAuth.IssueManagement.Issue
import qualified API.Action.DashboardAuth.IssueManagement.IssueList
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Servant

type API = (API.Action.DashboardAuth.IssueManagement.Issue.API :<|> API.Action.DashboardAuth.IssueManagement.IssueList.API)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = API.Action.DashboardAuth.IssueManagement.Issue.handler merchantId city :<|> API.Action.DashboardAuth.IssueManagement.IssueList.handler merchantId city
