{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.Search
  ( API,
    handler,
  )
where

import qualified API.Types.UI.Search
import qualified Control.Lens
import qualified Data.Text
import qualified Domain.Action.UI.Search as Domain.Action.UI.Search
import qualified Domain.Types.Client
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Types.Version
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( TokenAuth :> "rideSearch" :> "v2" :> Header "x-bundle-version" Kernel.Types.Version.Version :> Header "x-config-version" Kernel.Types.Version.Version
      :> Header
           "x-client-version"
           Kernel.Types.Version.Version
      :> Header "client-id" (Kernel.Types.Id.Id Domain.Types.Client.Client)
      :> Header
           "x-device"
           Data.Text.Text
      :> Header
           "is-dashboard-request"
           Kernel.Prelude.Bool
      :> ReqBody
           '[JSON]
           API.Types.UI.Search.SearchReqV2
      :> Post
           '[JSON]
           API.Types.UI.Search.SearchResp
  )

handler :: Environment.FlowServer API
handler = postRideSearchV2

postRideSearchV2 ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Maybe Kernel.Types.Version.Version ->
    Kernel.Prelude.Maybe Kernel.Types.Version.Version ->
    Kernel.Prelude.Maybe Kernel.Types.Version.Version ->
    Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Client.Client) ->
    Kernel.Prelude.Maybe Data.Text.Text ->
    Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
    API.Types.UI.Search.SearchReqV2 ->
    Environment.FlowHandler API.Types.UI.Search.SearchResp
  )
postRideSearchV2 a8 a7 a6 a5 a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.Search.postRideSearchV2 (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a8) a7 a6 a5 a4 a3 a2 a1
