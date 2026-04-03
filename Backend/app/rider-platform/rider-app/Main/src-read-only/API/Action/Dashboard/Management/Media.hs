{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.Dashboard.Management.Media 
( API.Types.RiderPlatform.Management.Media.API,
handler )
where
import EulerHS.Prelude
import Servant
import Tools.Auth
import Kernel.Utils.Common
import qualified Domain.Action.Dashboard.Media
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Id
import qualified Kernel.Types.Beckn.Context
import qualified Data.Text
import qualified API.Types.RiderPlatform.Management.Media



handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.RiderPlatform.Management.Media.API)
handler merchantId city = getMediaFile merchantId city
getMediaFile :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Data.Text.Text -> Environment.FlowHandler API.Types.RiderPlatform.Management.Media.GetMediaFileResponse)
getMediaFile a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Media.getMediaFile a3 a2 a1



