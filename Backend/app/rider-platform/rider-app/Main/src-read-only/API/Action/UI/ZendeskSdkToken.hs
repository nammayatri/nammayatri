{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.ZendeskSdkToken
  ( API,
    handler,
  )
where

import qualified Control.Lens
import qualified Domain.Action.UI.ZendeskSdkToken
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

type API = (TokenAuth :> "profile" :> "zendeskSdkToken" :> Get ('[JSON]) Kernel.Prelude.Text)

handler :: Environment.FlowServer API
handler = getProfileZendeskSdkToken

getProfileZendeskSdkToken :: ((Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Environment.FlowHandler Kernel.Prelude.Text)
getProfileZendeskSdkToken a1 = withFlowHandlerAPI $ Domain.Action.UI.ZendeskSdkToken.getProfileZendeskSdkToken (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)
