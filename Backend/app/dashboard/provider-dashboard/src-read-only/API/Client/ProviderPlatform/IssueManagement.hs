{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Client.ProviderPlatform.IssueManagement where

import qualified "dynamic-offer-driver-app" API.Dashboard
import qualified API.Types.ProviderPlatform.IssueManagement.Issue
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Domain.Types.ServerName
import Kernel.Prelude
import qualified Kernel.Types.Beckn.City
import Servant
import qualified "lib-dashboard" Tools.Auth.Merchant
import qualified "lib-dashboard" Tools.Client

newtype IssueManagementAPIs = IssueManagementAPIs {issueDSL :: API.Types.ProviderPlatform.IssueManagement.Issue.IssueAPIs}

mkIssueManagementAPIs :: (Tools.Auth.Merchant.CheckedShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.City.City -> Text -> IssueManagementAPIs)
mkIssueManagementAPIs merchantId city token = do let { issueDSL = API.Types.ProviderPlatform.IssueManagement.Issue.mkIssueAPIs issueClientDSL }; (IssueManagementAPIs {..})
  where
    issueClientDSL = Tools.Client.clientWithMerchantAndCity (Proxy :: Proxy API.Dashboard.IssueManagementDSLAPI) merchantId city token

callIssueManagementAPI ::
  forall m r b c.
  Tools.Client.DashboardClient IssueManagementAPIs m r b c =>
  (Tools.Auth.Merchant.CheckedShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.City.City -> (IssueManagementAPIs -> b) -> c)
callIssueManagementAPI merchantId city = Tools.Client.callServerAPI @_ @m @r Domain.Types.ServerName.DRIVER_OFFER_BPP_MANAGEMENT (mkIssueManagementAPIs merchantId city) "callIssueManagementAPI"
