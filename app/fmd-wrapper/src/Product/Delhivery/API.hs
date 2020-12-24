module Product.Delhivery.API where

import qualified Beckn.Types.FMD.API.Confirm as API
import qualified Beckn.Types.FMD.API.Init as API
import qualified Beckn.Types.FMD.API.Search as API
import qualified Beckn.Types.FMD.API.Select as API
import Beckn.Utils.Servant.HeaderAuth
import EulerHS.Prelude
import Servant

onSearchAPI :: Proxy (APIKeyAuth v :> API.OnSearchAPI)
onSearchAPI = Proxy

onSelectAPI :: Proxy (APIKeyAuth v :> API.OnSelectAPI)
onSelectAPI = Proxy

onInitAPI :: Proxy (APIKeyAuth v :> API.OnInitAPI)
onInitAPI = Proxy

onConfirmAPI :: Proxy (APIKeyAuth v :> API.OnConfirmAPI)
onConfirmAPI = Proxy
