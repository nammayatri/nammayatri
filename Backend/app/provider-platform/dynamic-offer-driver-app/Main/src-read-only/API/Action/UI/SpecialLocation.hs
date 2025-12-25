{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.SpecialLocation
  ( API,
    handler,
  )
where

import qualified Control.Lens
import qualified Domain.Action.UI.SpecialLocation
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Queries.SpecialLocation
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API = (TokenAuth :> "specialLocation" :> "list" :> QueryParam "isOrigin" Kernel.Prelude.Bool :> Get '[JSON] [Lib.Queries.SpecialLocation.SpecialLocationFull])

handler :: Environment.FlowServer API
handler = getSpecialLocationList

getSpecialLocationList ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
    Environment.FlowHandler [Lib.Queries.SpecialLocation.SpecialLocationFull]
  )
getSpecialLocationList a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.SpecialLocation.getSpecialLocationList (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
