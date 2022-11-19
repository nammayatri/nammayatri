{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Core.Beckn where

import qualified Beckn.Storage.Esqueleto as Esq
import qualified Beckn.Storage.Queries.BecknRequest as QBR
import Beckn.Types.Id
import Beckn.Utils.Callback (WithBecknCallbackMig, withBecknCallbackMig)
import Beckn.Utils.Common
import Beckn.Utils.Servant.SignatureAuth
import Control.Lens.Operators ((?~))
import Data.List (lookup)
import qualified Data.Text.Encoding as T
import Domain.Types.Merchant as DM
import Environment
import EulerHS.Prelude
import qualified Network.Wai.Internal as Wai
import Servant
import SharedLogic.CallBAP (buildBppUrl)

withCallback ::
  HasFlowEnv m r '["nwAddress" ::: BaseUrl, "httpClientOptions" ::: HttpClientOptions] =>
  DM.Merchant ->
  WithBecknCallbackMig api callback_success m
withCallback = withCallback' withRetry

withCallback' ::
  (m () -> m ()) ->
  HasFlowEnv m r '["nwAddress" ::: BaseUrl] =>
  DM.Merchant ->
  WithBecknCallbackMig api callback_success m
withCallback' doWithCallback transporter action api context cbUrl f = do
  let bppSubscriberId = getShortId $ transporter.subscriberId
      authKey = getHttpManagerKey bppSubscriberId
  bppUri <- buildBppUrl (transporter.id)
  let context' =
        context
          & #bpp_uri ?~ bppUri
          & #bpp_id ?~ bppSubscriberId
  withBecknCallbackMig doWithCallback (Just authKey) action api context' cbUrl f

logBecknRequest :: AppEnv -> Application -> Application
logBecknRequest appEnv f req@Wai.Request {..} respF = do
  req' <- case lookup "Authorization" requestHeaders of
    Nothing -> return req
    Just header -> do
      body <- requestBody
      bodyMvar <- newMVar body
      let logEnv = appEnv.loggerEnv
          esqDBEnv = appEnv.esqDBEnv
      Esq.runTransactionIO logEnv esqDBEnv $ do
        QBR.logBecknRequest (T.decodeUtf8 body) (T.decodeUtf8 header)
      return req {Wai.requestBody = mkRequestBody bodyMvar}
  f req' respF
  where
    mkRequestBody mvar = tryTakeMVar mvar <&> fromMaybe mempty
