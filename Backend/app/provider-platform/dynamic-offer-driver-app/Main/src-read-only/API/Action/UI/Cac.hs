{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.Cac
  ( API,
    handler,
  )
where

import qualified Control.Lens
import qualified Data.Aeson
import qualified Domain.Action.UI.Cac
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
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
  ( TokenAuth :> "driver" :> "getUiConfigs" :> QueryParam "toss" Kernel.Prelude.Int :> QueryParam "tenant" Kernel.Prelude.Text :> ReqBody '[JSON] Data.Aeson.Object
      :> Post
           '[JSON]
           Data.Aeson.Object
  )

handler :: Environment.FlowServer API
handler = postDriverGetUiConfigs

postDriverGetUiConfigs ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Kernel.Prelude.Maybe Kernel.Prelude.Text ->
    Data.Aeson.Object ->
    Environment.FlowHandler Data.Aeson.Object
  )
postDriverGetUiConfigs a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.Cac.postDriverGetUiConfigs (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a4) a3 a2 a1
