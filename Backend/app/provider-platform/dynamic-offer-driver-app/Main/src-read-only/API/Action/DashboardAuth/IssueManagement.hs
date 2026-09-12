{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.IssueManagement
  ( API,
    handler,
  )
where

import qualified API.Action.DashboardAuth.IssueManagement.Issue
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Servant

type API = API.Action.DashboardAuth.IssueManagement.Issue.API

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = API.Action.DashboardAuth.IssueManagement.Issue.handler merchantId city
