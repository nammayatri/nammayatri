{-# LANGUAGE TypeApplications #-}

module ExternalAPI where

import Beckn.Prelude
import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.ReqTypes
import qualified Control.Monad.Catch as C
import Core.OnCancel
import Core.OnConfirm
import Core.OnInit
import Core.OnSearch
import Core.OnStatus
import qualified Data.ByteString as BS
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Types.Header
import Servant
import Servant.Client
import Types.App

type GatewayOnSearchAPI =
  "v1"
    :> "on_search"
    :> ReqBody '[JSON] (BecknCallbackReq OnSearchCatalog)
    :> Post '[JSON] AckResponse

callGatewayOnSearchS :: BecknCallbackReq OnSearchCatalog -> MockM ()
callGatewayOnSearchS req = do
  let gatewayUrl = BaseUrl Http "localhost" 8015 ""
      clientFunc = client $ Proxy @GatewayOnSearchAPI
      clientAction = clientFunc req
  _ <- callAPI gatewayUrl clientAction
  pure ()

----------------------------

type OnInitAPI =
  "on_init"
    :> ReqBody '[JSON] (BecknCallbackReq OnInitMessage)
    :> Post '[JSON] AckResponse

callBapOnInit :: BaseUrl -> BecknCallbackReq OnInitMessage -> MockM ()
callBapOnInit bapUrl req = do
  let clientFunc = client $ Proxy @OnInitAPI
      clientAction = clientFunc req
  _ <- callAPI bapUrl clientAction
  pure ()

----------------------------

type OnConfirmAPI =
  "on_confirm"
    :> ReqBody '[JSON] (BecknCallbackReq OnConfirmMessage)
    :> Post '[JSON] AckResponse

callBapOnConfirmS :: BaseUrl -> BecknCallbackReq OnConfirmMessage -> MockM ()
callBapOnConfirmS bapUrl req = do
  let clientFunc = client $ Proxy @OnConfirmAPI
      clientAction = clientFunc req
  _ <- callAPI bapUrl clientAction
  pure ()

----------------------------

type OnStatusAPI =
  "on_status"
    :> ReqBody '[JSON] (BecknCallbackReq OnStatusMessage)
    :> Post '[JSON] AckResponse

callBapOnStatus :: BecknCallbackReq OnStatusMessage -> MockM ()
callBapOnStatus req = do
  let bapUrl = req.context.bap_uri
      clientFunc = client $ Proxy @OnStatusAPI
      clientAction = clientFunc req
  _ <- callAPI bapUrl clientAction
  pure ()

----------------------------

type OnCancelAPI =
  "on_cancel"
    :> ReqBody '[JSON] (BecknCallbackReq OnCancelMessage)
    :> Post '[JSON] AckResponse

callBapOnCancel :: BecknCallbackReq OnCancelMessage -> MockM ()
callBapOnCancel req = do
  let bapUrl = req.context.bap_uri
      clientFunc = client $ Proxy @OnCancelAPI
      clientAction = clientFunc req
  _ <- callAPI bapUrl clientAction
  pure ()

----------------------------
callAPI :: BaseUrl -> ClientM a -> MockM a
callAPI url clientAction = do
  subscriberId <- asks (.selfId)
  uniqueKey <- asks (.uniqueKeyId)
  liftIO $ do
    let fakeSignature = buildFakeSignature subscriberId uniqueKey
        modifyRequestFunc req = pure req {requestHeaders = (hAuthorization, fakeSignature) : requestHeaders req}
        managerSettings = defaultManagerSettings {managerModifyRequest = modifyRequestFunc}
    manager <- newManager managerSettings
    res <- runClientM clientAction $ mkClientEnv manager url
    case res of
      Left err -> C.throwM err
      Right a -> pure a

buildFakeSignature :: Text -> Text -> BS.ByteString
buildFakeSignature subscriberId uniqueKey =
  "Signature keyId=\""
    <> encodeUtf8 subscriberId
    <> "|"
    <> encodeUtf8 uniqueKey
    <> "|ed25519\",algorithm=\"ed25519\",created=1639662887,expires=1639663487,headers=\"(created) (expires) digest\",signature=\"fake\""
