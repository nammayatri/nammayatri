{-# LANGUAGE TypeApplications #-}

module Common.ExternalAPI where

import Beckn.Types.Core.ReqTypes
import Common.App
import qualified Control.Monad.Catch as C
import qualified Data.ByteString as BS
import Data.String.Conversions
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Types.Header
import Relude
import Servant.Client

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
    either C.throwM pure res

--------------------
buildFakeSignature :: Text -> Text -> BS.ByteString
buildFakeSignature subscriberId uniqueKey =
  "Signature keyId=\""
    <> cs subscriberId
    <> "|"
    <> cs uniqueKey
    <> "|ed25519\",algorithm=\"ed25519\",created=1639662887,expires=1639663487,headers=\"(created) (expires) digest\",signature=\"fake\""
