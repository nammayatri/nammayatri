{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
-- requestBody record update is the only way to re-attach a buffered body on this wai version
{-# OPTIONS_GHC -Wno-deprecations #-}

module Utils.Common.CrossCloudProxy
  ( crossCloudProxyMiddleware,
    forwardedFromHeader,
  )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.IORef (atomicModifyIORef, newIORef)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified EulerHS.Runtime as R
import Kernel.Prelude hiding (app)
import Kernel.Types.Flow
import Kernel.Types.Version (CloudType (..))
import Kernel.Utils.Common
import Kernel.Utils.IOLogging (HasLog)
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Types as HttpTypes
import qualified Network.Wai as Wai
import Network.Wai.Internal (Request (..))

forwardedFromHeader :: HttpTypes.HeaderName
forwardedFromHeader = "x-ny-forwarded-from"

-- | Forwards a UI request landing on the wrong cloud to the cloud owning the
-- person's merchant operating city and relays the response back. Fail-open:
-- any error in resolution or forwarding serves the request locally.
crossCloudProxyMiddleware ::
  HasLog env =>
  R.FlowRuntime ->
  env ->
  CloudType ->
  [Text] ->
  Http.Manager ->
  (Text -> FlowR env (Maybe (CloudType, BaseUrl))) ->
  Wai.Middleware
crossCloudProxyMiddleware flowRt appEnv podCloud uiPrefixes manager resolveOwner app req respond
  | not isEligiblePath = app req respond
  | isJust (lookup forwardedFromHeader (Wai.requestHeaders req)) = app req respond
  | otherwise =
    case lookup "token" (Wai.requestHeaders req) of
      Nothing -> app req respond
      Just token -> do
        mbTarget <-
          runFlowR flowRt appEnv (withLogTag "CrossCloudProxy" $ resolveOwner (TE.decodeUtf8 token))
            `catch` \(SomeException _) -> pure Nothing
        case mbTarget of
          Just (ownerCloud, ownerUrl)
            | ownerCloud /= podCloud && ownerCloud /= UNAVAILABLE ->
              forward ownerCloud ownerUrl
          _ -> app req respond
  where
    isEligiblePath = case Wai.pathInfo req of
      (firstSegment : _) -> firstSegment `elem` uiPrefixes
      [] -> False

    forward ownerCloud ownerUrl = do
      reqBody <- Wai.strictRequestBody req
      result <- try @_ @SomeException $ do
        baseReq <- Http.parseRequest (T.unpack $ showBaseUrl ownerUrl)
        let basePath = if "/" `BS.isSuffixOf` Http.path baseReq then BS.init (Http.path baseReq) else Http.path baseReq
            fullPath = basePath <> Wai.rawPathInfo req
            fwdHeaders =
              filter (\(h, _) -> h `notElem` requestHopHeaders) (Wai.requestHeaders req)
                <> [(forwardedFromHeader, TE.encodeUtf8 . T.pack $ show podCloud)]
            fwdReq =
              baseReq
                { Http.method = Wai.requestMethod req,
                  Http.path = fullPath,
                  Http.queryString = Wai.rawQueryString req,
                  Http.requestHeaders = fwdHeaders,
                  Http.requestBody = Http.RequestBodyLBS reqBody,
                  Http.redirectCount = 0,
                  Http.responseTimeout = Http.responseTimeoutMicro 60000000
                }
        Http.httpLbs fwdReq manager
      case result of
        Right resp -> do
          runFlowR flowRt appEnv . withLogTag "CrossCloudProxy" $
            logInfo $ "Forwarded " <> TE.decodeUtf8 (Wai.rawPathInfo req) <> " from " <> show podCloud <> " to " <> show ownerCloud <> ", status: " <> show (HttpTypes.statusCode $ Http.responseStatus resp)
          let respHeaders = filter (\(h, _) -> h `notElem` responseHopHeaders) (Http.responseHeaders resp)
          respond $ Wai.responseLBS (Http.responseStatus resp) respHeaders (Http.responseBody resp)
        Left err -> do
          runFlowR flowRt appEnv . withLogTag "CrossCloudProxy" $
            logError $ "Forward to " <> show ownerCloud <> " failed, serving locally: " <> show err
          replayableReq <- replayBody reqBody
          app replayableReq respond

    replayBody reqBody = do
      chunksRef <- newIORef (BSL.toChunks reqBody)
      let readChunk =
            atomicModifyIORef chunksRef $ \case
              [] -> ([], BS.empty)
              (c : cs) -> (cs, c)
      pure req {requestBody = readChunk}

    requestHopHeaders :: [HttpTypes.HeaderName]
    requestHopHeaders = ["host", "content-length", "transfer-encoding", "connection", "accept-encoding", "expect"]

    responseHopHeaders :: [HttpTypes.HeaderName]
    responseHopHeaders = ["content-length", "transfer-encoding", "connection", "content-encoding"]
