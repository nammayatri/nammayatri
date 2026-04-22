module API.UI.SVP
  ( API,
    handler,
  )
where

import qualified Domain.Action.UI.SVP as DSVP
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import Environment
import EulerHS.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth (TokenAuth)
import Tools.FlowHandling (withFlowHandlerAPIPersonId)

type API =
  "svp"
    :> ( "sign-qr"
           :> TokenAuth
           :> ReqBody '[JSON] DSVP.SignQRReq
           :> Post '[JSON] DSVP.SignQRResp
           :<|> "public-key"
             :> TokenAuth
             :> Get '[PlainText] Text
           :<|> "rotate-keys"
             :> TokenAuth
             :> Post '[JSON] DSVP.RotateKeysResp
       )

handler :: FlowServer API
handler =
  signQRHandler
    :<|> publicKeyHandler
    :<|> rotateKeysHandler

signQRHandler ::
  (Id Person.Person, Id Merchant.Merchant) ->
  DSVP.SignQRReq ->
  FlowHandler DSVP.SignQRResp
signQRHandler (personId, _) req =
  withFlowHandlerAPIPersonId personId . withPersonIdLogTag personId $
    DSVP.signQR req

publicKeyHandler ::
  (Id Person.Person, Id Merchant.Merchant) ->
  FlowHandler Text
publicKeyHandler (personId, _) =
  withFlowHandlerAPIPersonId personId . withPersonIdLogTag personId $
    DSVP.getPublicKey

rotateKeysHandler ::
  (Id Person.Person, Id Merchant.Merchant) ->
  FlowHandler DSVP.RotateKeysResp
rotateKeysHandler (personId, _) =
  withFlowHandlerAPIPersonId personId . withPersonIdLogTag personId $
    DSVP.rotateKeys
