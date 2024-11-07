{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.IssueManagement where

import qualified API.Action.ProviderPlatform.IssueManagement.Issue
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Servant

type API = API.Action.ProviderPlatform.IssueManagement.Issue.API

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = API.Action.ProviderPlatform.IssueManagement.Issue.handler merchantId city
