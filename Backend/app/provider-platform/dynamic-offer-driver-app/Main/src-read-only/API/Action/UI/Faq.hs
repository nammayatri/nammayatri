{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.Faq
  ( API,
    handler,
  )
where

import qualified API.Types.UI.Faq
import qualified Control.Lens
import qualified Domain.Action.UI.Faq
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

type API = (TokenAuth :> "faq" :> QueryParam "category" Kernel.Prelude.Text :> Get ('[JSON]) [API.Types.UI.Faq.FaqAPIEntity])

handler :: Environment.FlowServer API
handler = getFaq

getFaq ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Text) ->
    Environment.FlowHandler [API.Types.UI.Faq.FaqAPIEntity]
  )
getFaq a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.Faq.getFaq (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
