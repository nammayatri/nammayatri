{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.Dashboard.Management.SosMedia 
( API.Types.ProviderPlatform.Management.SosMedia.API,
handler )
where
import EulerHS.Prelude
import Servant
import Tools.Auth
import Kernel.Utils.Common
import qualified Domain.Action.Dashboard.Management.SosMedia
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Id
import qualified Kernel.Types.Beckn.Context
import qualified Dashboard.Common
import qualified API.Types.ProviderPlatform.Management.SosMedia



handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Management.SosMedia.API)
handler merchantId city = getSosMediaSosMedia merchantId city
getSosMediaSosMedia :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler [API.Types.ProviderPlatform.Management.SosMedia.GetSosMediaResponse])
getSosMediaSosMedia a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.SosMedia.getSosMediaSosMedia a3 a2 a1



