{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.FollowRide where

import API.Types.UI.FollowRide (Followers)
import qualified API.Types.UI.FollowRide
import qualified Domain.Action.UI.FollowRide as Domain.Action.UI.FollowRide
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  TokenAuth :> "follow" :> "ride" :> Get '[JSON] [API.Types.UI.FollowRide.Followers]

handler :: Environment.FlowServer API
handler = getFollowRide

getFollowRide :: (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Environment.FlowHandler [API.Types.UI.FollowRide.Followers]
getFollowRide a1 = withFlowHandlerAPI $ Domain.Action.UI.FollowRide.getFollowRide a1
