module Product.Dunzo.API where

import qualified Beckn.Types.FMD.API.Cancel as API
import qualified Beckn.Types.FMD.API.Confirm as API
import qualified Beckn.Types.FMD.API.Init as API
import qualified Beckn.Types.FMD.API.Search as API
import qualified Beckn.Types.FMD.API.Select as API
import qualified Beckn.Types.FMD.API.Status as API
import qualified Beckn.Types.FMD.API.Track as API
import qualified Beckn.Types.FMD.API.Update as API
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

onTrackAPI :: Proxy (APIKeyAuth v :> API.OnTrackAPI)
onTrackAPI = Proxy

onStatusAPI :: Proxy (APIKeyAuth v :> API.OnStatusAPI)
onStatusAPI = Proxy

onCancelAPI :: Proxy (APIKeyAuth v :> API.OnCancelAPI)
onCancelAPI = Proxy

onUpdateAPI :: Proxy (APIKeyAuth v :> API.OnUpdateAPI)
onUpdateAPI = Proxy
