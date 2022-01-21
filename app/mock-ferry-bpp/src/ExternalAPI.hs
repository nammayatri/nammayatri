{-# LANGUAGE TypeApplications #-}

module ExternalAPI where

import Beckn.Prelude
import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.ReqTypes
import qualified Control.Monad.Catch as C
import Core.OnCancel
import Core.OnConfirm
import Core.OnSearch
import Core.OnStatus
import qualified Data.ByteString as BS
import Data.String.Conversions
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

callGatewayOnSearch :: BecknCallbackReq OnSearchCatalog -> MockM ()
callGatewayOnSearch = callAPI @GatewayOnSearchAPI gatewayUrl
  where
    gatewayUrl = BaseUrl Http "localhost" 8015 ""

----------------------------
type OnConfirmAPI =
  "on_confirm"
    :> ReqBody '[JSON] (BecknCallbackReq OnConfirmMessage)
    :> Post '[JSON] AckResponse

callBapOnConfirm :: BecknCallbackReq OnConfirmMessage -> MockM ()
callBapOnConfirm = callBapAPI @OnConfirmAPI

----------------------------
type OnStatusAPI =
  "on_status"
    :> ReqBody '[JSON] (BecknCallbackReq OnStatusMessage)
    :> Post '[JSON] AckResponse

callBapOnStatus :: BecknCallbackReq OnStatusMessage -> MockM ()
callBapOnStatus = callBapAPI @OnStatusAPI

----------------------------
type OnCancelAPI =
  "on_cancel"
    :> ReqBody '[JSON] (BecknCallbackReq OnCancelMessage)
    :> Post '[JSON] AckResponse

callBapOnCancel :: BecknCallbackReq OnCancelMessage -> MockM ()
callBapOnCancel = callBapAPI @OnCancelAPI

----------------------------
callBapAPI ::
  forall api a b.
  ( HasClient ClientM api,
    Client ClientM api ~ (BecknCallbackReq a -> ClientM b)
  ) =>
  BecknCallbackReq a ->
  MockM ()
callBapAPI req = do
  let bapUrl = req.context.bap_uri
  callAPI @api bapUrl req

callAPI ::
  forall api a b.
  ( HasClient ClientM api,
    Client ClientM api ~ (BecknCallbackReq a -> ClientM b)
  ) =>
  BaseUrl ->
  BecknCallbackReq a ->
  MockM ()
callAPI url req = do
  let clientFunc = client @api Proxy
      clientAction = clientFunc req
  _ <- callClientM url clientAction
  pure ()

callClientM :: BaseUrl -> ClientM a -> MockM a
callClientM url clientAction = do
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

--------------------
buildFakeSignature :: Text -> Text -> BS.ByteString
buildFakeSignature subscriberId uniqueKey =
  "Signature keyId=\""
    <> cs subscriberId
    <> "|"
    <> cs uniqueKey
    <> "|ed25519\",algorithm=\"ed25519\",created=1639662887,expires=1639663487,headers=\"(created) (expires) digest\",signature=\"fake\""
