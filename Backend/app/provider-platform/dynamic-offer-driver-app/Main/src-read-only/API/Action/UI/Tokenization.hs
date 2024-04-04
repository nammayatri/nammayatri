{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.Tokenization where

import qualified API.Types.UI.Tokenization
import qualified Control.Lens
import qualified Domain.Action.UI.Tokenization as Domain.Action.UI.Tokenization
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.External.Tokenize.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( TokenAuth :> "driver" :> "getToken" :> MandatoryQueryParam "expiry" Kernel.Prelude.Int :> MandatoryQueryParam "service" Kernel.External.Tokenize.Types.TokenizationService
      :> Get
           ('[JSON])
           API.Types.UI.Tokenization.GetTokenRes
  )

handler :: Environment.FlowServer API
handler = getDriverGetToken

getDriverGetToken ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Int ->
    Kernel.External.Tokenize.Types.TokenizationService ->
    Environment.FlowHandler API.Types.UI.Tokenization.GetTokenRes
  )
getDriverGetToken a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.Tokenization.getDriverGetToken (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1
