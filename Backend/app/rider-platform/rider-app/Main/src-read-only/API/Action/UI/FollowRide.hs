{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.FollowRide where

import API.Types.UI.FollowRide (Followers, ShareRideReq)
import qualified API.Types.UI.FollowRide
import qualified Domain.Action.UI.FollowRide as Domain.Action.UI.FollowRide
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  TokenAuth :> "follow" :> "ride" :> Get '[JSON] [API.Types.UI.FollowRide.Followers]
    :<|> TokenAuth :> "share" :> "ride" :> ReqBody '[JSON] API.Types.UI.FollowRide.ShareRideReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess

handler :: Environment.FlowServer API
handler =
  getFollowRide
    :<|> postShareRide

getFollowRide :: (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Environment.FlowHandler [API.Types.UI.FollowRide.Followers]
getFollowRide a1 = withFlowHandlerAPI $ Domain.Action.UI.FollowRide.getFollowRide (Kernel.Prelude.first Kernel.Prelude.Just a1)

postShareRide :: (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> API.Types.UI.FollowRide.ShareRideReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
postShareRide a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.FollowRide.postShareRide (Kernel.Prelude.first Kernel.Prelude.Just a2) a1
