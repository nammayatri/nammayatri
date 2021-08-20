{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-warnings-deprecations #-}

module Beckn.Utils.App
  ( handleLeft,
    handleShutdown,
    logRequestAndResponse,
    withModifiedEnv,
    hashBodyForSignature,
  )
where

import Beckn.Types.App
import Beckn.Types.Flow
import Beckn.Utils.Common
import Beckn.Utils.FlowLogging (appendLogContext)
import qualified Beckn.Utils.SignatureAuth as HttpSig
import Control.Concurrent.STM.TMVar
import qualified Data.ByteArray as BA
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LB
import qualified Data.CaseInsensitive as CI
import Data.List (lookup)
import Data.UUID.V4 (nextRandom)
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (unpack)
import Network.HTTP.Types (Method, RequestHeaders)
import qualified Network.HTTP.Types as HTTP
import Network.Wai
import qualified Network.Wai as Wai
import Network.Wai.Internal
import System.Exit (ExitCode)
import System.Posix.Signals (Handler (Catch), installHandler, sigINT, sigTERM)

data RequestInfo = RequestInfo
  { requestMethod :: Method,
    rawPathInfo :: ByteString,
    rawQueryString :: ByteString,
    requestHeaders :: RequestHeaders
  }
  deriving (Show)

data ResponseInfo = ResponseInfo
  { statusCode :: Int,
    statusMessage :: Text,
    headers :: [(Text, Text)]
  }
  deriving (Show)

handleLeft :: forall a b m. (Show a, Log m, L.MonadFlow m) => ExitCode -> Text -> Either a b -> m b
handleLeft exitCode msg = \case
  Left err -> do
    logError (msg <> show err)
    L.runIO $ exitWith exitCode
  Right res -> return res

handleShutdown :: TMVar () -> IO () -> IO ()
handleShutdown shutdown closeSocket = do
  void $ installHandler sigTERM (Catch $ shutdownAction "sigTERM" >> closeSocket) Nothing
  void $ installHandler sigINT (Catch $ shutdownAction "sigINT" >> closeSocket) Nothing
  where
    shutdownAction reason = do
      isLocked <- atomically $ do
        isEmptyTMVar shutdown >>= \case
          True -> do
            putTMVar shutdown ()
            return True
          False -> return False
      when isLocked $ do
        putStrLn @String $ "Shutting down by " <> reason

hashBodyForSignature :: Application -> Application
hashBodyForSignature f req respF = do
  req' <-
    if anyAuthHeaders
      then do
        body <- strictRequestBody req <&> LB.toStrict
        mvar <- newMVar body
        let requestHeaders =
              ( HttpSig.bodyHashHeader,
                Base64.encode $ BA.convert $ HttpSig.becknSignatureHash body
              ) :
              Wai.requestHeaders req
        pure req {requestBody = mkRequestBody mvar, requestHeaders}
      else pure req
  f req' respF
  where
    mkRequestBody mvar = tryTakeMVar mvar <&> fromMaybe B.empty
    headers = map fst $ Wai.requestHeaders req
    anyAuthHeaders = any (`elem` headers) ["Authorization", "Proxy-Authorization", "Signature"]

logRequestAndResponse :: EnvR f -> Application -> Application
logRequestAndResponse (EnvR flowRt appEnv) f req respF =
  f req loggedRespF
  where
    logInfoIO tag info = runFlowR flowRt appEnv $ logTagInfo tag info
    toRequestInfo Request {..} = RequestInfo {..}
    toResponseInfo resp =
      let (status, headers, _) = responseToStream resp
          code = HTTP.statusCode status
          decodeHeader = bimap (decodeUtf8 . CI.original) decodeUtf8
          respInfo =
            ResponseInfo
              { statusCode = code,
                statusMessage = decodeUtf8 $ HTTP.statusMessage status,
                headers = decodeHeader <$> headers
              }
       in if code >= 300 then show respInfo else "Successful response"
    loggedRespF resp = do
      let respLogText = toResponseInfo resp
      logInfoIO "Request&Response" $ "Request: " <> show (toRequestInfo req) <> " || Response: " <> respLogText
      respF resp

withModifiedEnv :: (EnvR f -> Application) -> EnvR f -> Application
withModifiedEnv f env = \req resp -> do
  requestId <- getRequestId $ Wai.requestHeaders req
  let modifiedEnv = modifyEnvR requestId
  let app = f modifiedEnv
  app req resp
  where
    modifyEnvR requestId = env {flowRuntime = L.updateLoggerContext (appendLogContext requestId) $ flowRuntime env}
    getRequestId headers = do
      let value = lookup "x-request-id" headers
      case value of
        Just val -> pure ("requestId-" <> decodeUtf8 val)
        Nothing -> pure "randomRequestId-" <> show <$> nextRandom
