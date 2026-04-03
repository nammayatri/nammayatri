{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.Dashboard.Management.Media 
( API.Types.ProviderPlatform.Management.Media.API,
handler )
where
import EulerHS.Prelude
import Servant
import Tools.Auth
import Kernel.Utils.Common
import qualified Domain.Action.Dashboard.Management.Media
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Id
import qualified Kernel.Types.Beckn.Context
import qualified "shared-services" IssueManagement.Domain.Types.MediaFile
import qualified API.Types.ProviderPlatform.Management.Media



handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Management.Media.API)
handler merchantId city = getMediaMediaImage merchantId city
getMediaMediaImage :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id IssueManagement.Domain.Types.MediaFile.MediaFile -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Media.GetImageResponse)
getMediaMediaImage a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Media.getMediaMediaImage a3 a2 a1



