{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Beckn.Mock.ExternalAPI where

import Beckn.Mock.App
import Beckn.Types.Common
import Beckn.Types.Core.ReqTypes
import Beckn.Utils.IOLogging
import Beckn.Utils.Logging
import qualified Control.Monad.Catch as C
import qualified Data.ByteString as BS
import Data.String.Conversions
import GHC.Records.Extra
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Types.Header
import Relude
import Servant.Client

callBapAPI ::
  forall api a b e c.
  ( Show a,
    HasInConfig e c "selfId" Text,
    HasInConfig e c "uniqueKeyId" Text,
    HasClient ClientM api,
    Client ClientM api ~ (BecknCallbackReq a -> ClientM b),
    HasLog e
  ) =>
  BecknCallbackReq a ->
  MockM e ()
callBapAPI req = do
  let bapUrl = req.context.bap_uri
  logOutput INFO "calling BAP"
  callAPI @api bapUrl req

callAPI ::
  forall api a b e c.
  ( Show a,
    HasInConfig e c "selfId" Text,
    HasInConfig e c "uniqueKeyId" Text,
    HasClient ClientM api,
    Client ClientM api ~ (BecknCallbackReq a -> ClientM b),
    HasLog e
  ) =>
  BaseUrl ->
  BecknCallbackReq a ->
  MockM e ()
callAPI url req = do
  let clientFunc = client @api Proxy
      clientAction = clientFunc req
  logOutput INFO $ mconcat ["calling ", show req.context.action, "; url=", show url]
  logOutput DEBUG $ show req
  _ <- callClientM url clientAction
  pure ()

callClientM ::
  ( HasInConfig e c "selfId" Text,
    HasInConfig e c "uniqueKeyId" Text
  ) =>
  BaseUrl ->
  ClientM a ->
  MockM e a
callClientM url clientAction = do
  subscriberId <- asks (.config.selfId)
  uniqueKey <- asks (.config.uniqueKeyId)
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
