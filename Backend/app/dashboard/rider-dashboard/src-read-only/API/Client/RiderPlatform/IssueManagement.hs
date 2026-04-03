{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
module API.Client.RiderPlatform.IssueManagement where
import Kernel.Prelude
import Servant
import qualified API.Types.RiderPlatform.IssueManagement.Issue
import qualified API.Types.RiderPlatform.IssueManagement.IssueList
import qualified Kernel.Types.Beckn.City
import qualified "rider-app" API.Dashboard
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Domain.Types.ServerName
import qualified "lib-dashboard" Tools.Auth.Merchant
import qualified "lib-dashboard" Tools.Client


data IssueManagementAPIs = IssueManagementAPIs {issueDSL :: API.Types.RiderPlatform.IssueManagement.Issue.IssueAPIs, issueListDSL :: API.Types.RiderPlatform.IssueManagement.IssueList.IssueListAPIs}
mkIssueManagementAPIs :: (Tools.Auth.Merchant.CheckedShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.City.City -> Text -> IssueManagementAPIs)
mkIssueManagementAPIs merchantId city token = do {let {issueDSL = API.Types.RiderPlatform.IssueManagement.Issue.mkIssueAPIs issueClientDSL};
                                                  let {issueListDSL = API.Types.RiderPlatform.IssueManagement.IssueList.mkIssueListAPIs issueListClientDSL};
                                                  (IssueManagementAPIs {..})}
                          where issueClientDSL :<|> issueListClientDSL = Tools.Client.clientWithMerchantAndCity (Proxy :: Proxy API.Dashboard.IssueManagementDSLAPI) merchantId city token
callIssueManagementAPI :: forall m r b c . Tools.Client.DashboardClient IssueManagementAPIs m r b c =>
                          (Tools.Auth.Merchant.CheckedShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.City.City -> (IssueManagementAPIs -> b) -> c)
callIssueManagementAPI merchantId city = Tools.Client.callServerAPI @_ @m @r Domain.Types.ServerName.APP_BACKEND_MANAGEMENT (mkIssueManagementAPIs merchantId city) "callIssueManagementAPI"



