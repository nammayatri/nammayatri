{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.UI.Tokenization 
( API,
handler )
where
import Storage.Beam.SystemConfigs ()
import EulerHS.Prelude
import Servant
import Tools.Auth
import Kernel.Utils.Common
import qualified Domain.Action.UI.Tokenization
import qualified Domain.Types.Person
import qualified Kernel.Prelude
import qualified Control.Lens
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Id
import qualified Kernel.External.Tokenize
import qualified API.Types.UI.Tokenization
import qualified Domain.Types.MerchantOperatingCity



type API = (TokenAuth :> "driver" :> "sdkToken" :> MandatoryQueryParam "expiry" Kernel.Prelude.Int :> MandatoryQueryParam "service" Kernel.External.Tokenize.TokenizationService :> Get ('[JSON])
                                                                                                                                                                                        API.Types.UI.Tokenization.GetTokenRes)
handler :: Environment.FlowServer API
handler = getDriverSdkToken
getDriverSdkToken :: ((Kernel.Types.Id.Id Domain.Types.Person.Person,
                       Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                       Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> Kernel.Prelude.Int -> Kernel.External.Tokenize.TokenizationService -> Environment.FlowHandler API.Types.UI.Tokenization.GetTokenRes)
getDriverSdkToken a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.Tokenization.getDriverSdkToken (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1



