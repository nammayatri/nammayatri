{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.SpecialLocation where

import qualified Control.Lens
import qualified Domain.Action.UI.SpecialLocation as Domain.Action.UI.SpecialLocation
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
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

type API =
  TokenAuth :> "specialLocation" :> "list" :> Get '[JSON] [Lib.Queries.SpecialLocation.SpecialLocationFull]

handler :: Environment.FlowServer API
handler = getSpecialLocationList

getSpecialLocationList :: (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant, Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity) -> Environment.FlowHandler [Lib.Queries.SpecialLocation.SpecialLocationFull]
getSpecialLocationList a1 = withFlowHandlerAPI $ Domain.Action.UI.SpecialLocation.getSpecialLocationList (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)
