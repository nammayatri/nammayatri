{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.IssueManagement where

import qualified API.Action.RiderPlatform.IssueManagement.Issue
import qualified API.Action.RiderPlatform.IssueManagement.IssueList
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Servant

type API = (API.Action.RiderPlatform.IssueManagement.Issue.API :<|> API.Action.RiderPlatform.IssueManagement.IssueList.API)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = API.Action.RiderPlatform.IssueManagement.Issue.handler merchantId city :<|> API.Action.RiderPlatform.IssueManagement.IssueList.handler merchantId city
