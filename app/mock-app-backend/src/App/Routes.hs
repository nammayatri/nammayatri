module App.Routes where

import App.Types
import Beckn.Types.App
import Beckn.Types.Core.Ack (AckResponse (..))
import qualified Beckn.Types.FMD.API.Cancel as API
import qualified Beckn.Types.FMD.API.Confirm as API
import qualified Beckn.Types.FMD.API.Init as API
import qualified Beckn.Types.FMD.API.Search as API
import qualified Beckn.Types.FMD.API.Select as API
import qualified Beckn.Types.FMD.API.Status as API
import qualified Beckn.Types.FMD.API.Track as API
import qualified Beckn.Types.FMD.API.Update as API
import Beckn.Utils.Servant.SignatureAuth
import EulerHS.Prelude
import qualified Product.Cancel as P
import qualified Product.Confirm as P
import qualified Product.Init as P
import qualified Product.Search as P
import qualified Product.Select as P
import qualified Product.Status as P
import qualified Product.Track as P
import qualified Product.Trigger as T
import qualified Product.Update as P
import Servant hiding (Context)
import Utils.Auth

type MockAppBackendAPI =
  "v1"
    :> ( Get '[JSON] Text
           :<|> TriggerAPI
           :<|> BecknAPI
       )

type BecknAPI =
  SignatureAuth "Authorization" :> API.OnSearchAPI
    :<|> SignatureAuth "Authorization" :> API.OnSelectAPI
    :<|> SignatureAuth "Authorization" :> API.OnInitAPI
    :<|> SignatureAuth "Authorization" :> API.OnConfirmAPI
    :<|> SignatureAuth "Authorization" :> API.OnTrackAPI
    :<|> SignatureAuth "Authorization" :> API.OnStatusAPI
    :<|> SignatureAuth "Authorization" :> API.OnCancelAPI
    :<|> SignatureAuth "Authorization" :> API.OnUpdateAPI

mockAppBackendAPI :: Proxy MockAppBackendAPI
mockAppBackendAPI = Proxy

mockAppBackendServer :: FlowServer MockAppBackendAPI
mockAppBackendServer =
  pure "Mock app backend is UP"
    :<|> triggerFlow
    :<|> withBecknAuth P.searchCb lookup
    :<|> withBecknAuth P.selectCb lookup
    :<|> withBecknAuth P.initCb lookup
    :<|> withBecknAuth P.confirmCb lookup
    :<|> withBecknAuth P.trackCb lookup
    :<|> withBecknAuth P.statusCb lookup
    :<|> withBecknAuth P.cancelCb lookup
    :<|> withBecknAuth P.updateCb lookup

type TriggerAPI =
  "trigger"
    :> ( MandatoryQueryParam "flow" T.TriggerFlow
           :> Get '[JSON] AckResponse
           :<|> "search"
             :> MandatoryQueryParam "flow" T.TriggerFlow
             :> Get '[JSON] AckResponse
           :<|> "track"
             :> "last"
             :> Get '[JSON] AckResponse
           :<|> "track"
             :> Capture "order_id" Text
             :> Get '[JSON] AckResponse
           :<|> "cancel"
             :> "last"
             :> Get '[JSON] AckResponse
           :<|> "cancel"
             :> Capture "order_id" Text
             :> Get '[JSON] AckResponse
           :<|> "update"
             :> "last"
             :> MandatoryQueryParam "mode" T.TriggerUpdateMode
             :> Get '[JSON] AckResponse
           :<|> "update"
             :> Capture "order_id" Text
             :> MandatoryQueryParam "mode" T.TriggerUpdateMode
             :> Get '[JSON] AckResponse
       )

triggerFlow :: FlowServer TriggerAPI
triggerFlow =
  T.triggerSearch
    :<|> T.triggerSearch
    :<|> T.triggerTrackForLast
    :<|> T.triggerTrack
    :<|> T.triggerCancelForLast
    :<|> T.triggerCancel
    :<|> T.triggerUpdateForLast
    :<|> T.triggerUpdate
