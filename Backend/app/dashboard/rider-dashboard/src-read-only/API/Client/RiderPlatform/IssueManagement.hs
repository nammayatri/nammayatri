{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Client.RiderPlatform.IssueManagement where

import qualified "rider-app" API.Dashboard
import qualified API.Types.RiderPlatform.IssueManagement.Issue
import qualified API.Types.RiderPlatform.IssueManagement.IssueList
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Domain.Types.ServerName
import Kernel.Prelude
import qualified Kernel.Types.Beckn.City
import Servant
import qualified "lib-dashboard" Tools.Auth.Merchant
import qualified "lib-dashboard" Tools.Client

data IssueManagementAPIs = IssueManagementAPIs {issueDSL :: API.Types.RiderPlatform.IssueManagement.Issue.IssueAPIs, issueListDSL :: API.Types.RiderPlatform.IssueManagement.IssueList.IssueListAPIs}

mkIssueManagementAPIs :: (Tools.Auth.Merchant.CheckedShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.City.City -> Text -> IssueManagementAPIs)
mkIssueManagementAPIs merchantId city token = do
  let issueDSL = API.Types.RiderPlatform.IssueManagement.Issue.mkIssueAPIs issueClientDSL
  let issueListDSL = API.Types.RiderPlatform.IssueManagement.IssueList.mkIssueListAPIs issueListClientDSL
  (IssueManagementAPIs {..})
  where
    issueClientDSL :<|> issueListClientDSL = Tools.Client.clientWithMerchantAndCity (Proxy :: Proxy API.Dashboard.IssueManagementDSLAPI) merchantId city token

callIssueManagementAPI ::
  forall m r b c.
  Tools.Client.DashboardClient IssueManagementAPIs m r b c =>
  (Tools.Auth.Merchant.CheckedShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.City.City -> (IssueManagementAPIs -> b) -> c)
callIssueManagementAPI merchantId city = Tools.Client.callServerAPI @_ @m @r Domain.Types.ServerName.APP_BACKEND_MANAGEMENT (mkIssueManagementAPIs merchantId city) "callIssueManagementAPI"
