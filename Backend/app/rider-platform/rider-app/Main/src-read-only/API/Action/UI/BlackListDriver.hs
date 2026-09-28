{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.BlackListDriver
  ( API,
    handler,
  )
where

import qualified API.Types.UI.BlackListDriver
import qualified Control.Lens
import qualified Data.Text
import qualified Domain.Action.UI.BlackListDriver
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
  ( TokenAuth :> "driver"
      :> "blackList"
      :> Capture
           "driverId"
           Data.Text.Text
      :> ReqBody '[JSON] API.Types.UI.BlackListDriver.BlackListDriverReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

handler :: Environment.FlowServer API
handler = postDriverBlackList

postDriverBlackList ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Data.Text.Text ->
    API.Types.UI.BlackListDriver.BlackListDriverReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postDriverBlackList a3 a1 a2 = withFlowHandlerAPI $ Domain.Action.UI.BlackListDriver.postDriverBlackList (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a1 a2
