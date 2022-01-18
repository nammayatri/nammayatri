module API.Types where

import qualified API.Cancel.Types as Cancel
import qualified API.Confirm.Types as Confirm
import qualified API.Search.Types as Search
import qualified API.Status.Types as Status
import qualified API.Swagger.Types as Swagger
import qualified API.Track.Types as Track
import Beckn.Utils.Servant.SignatureAuth
import EulerHS.Prelude
import Servant

type API =
  MainAPI
    :<|> Swagger.API

type MainAPI =
  "v1"
    :> ( Get '[JSON] Text
           :<|> SignatureAuth "Authorization" :> SignatureAuth "X-Gateway-Authorization" :> Search.API
           :<|> SignatureAuth "Authorization" :> Confirm.API
           :<|> SignatureAuth "Authorization" :> Status.API
           :<|> SignatureAuth "Authorization" :> Track.API
           :<|> SignatureAuth "Authorization" :> Cancel.API
       )
