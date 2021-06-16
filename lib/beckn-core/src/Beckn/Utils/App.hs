{-# LANGUAGE TypeApplications #-}

module Beckn.Utils.App
  ( handleLeft,
    handleShutdown,
    logRequestAndResponse,
    withModifiedEnv,
  )
where

import Beckn.Types.App
import Beckn.Utils.Common
import Control.Concurrent.STM.TMVar
import qualified Data.ByteString.Char8 as BS
import qualified Data.CaseInsensitive as CI
import Data.List (lookup)
import qualified Data.Text as T
import Data.UUID.V4 (nextRandom)
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (unpack)
import Network.HTTP.Types (Method, RequestHeaders)
import qualified Network.HTTP.Types as HTTP
import Network.Wai
import Network.Wai.Internal as Wai
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

logRequestAndResponse :: EnvR f -> Application -> Application
logRequestAndResponse (EnvR flowRt appEnv) f req respF = do
  logInfoIO "Request" . show $ toRequestInfo req
  f req loggedRespF
  where
    logInfoIO tag info = runFlowR flowRt appEnv $ logTagInfo tag info
    toRequestInfo Request {..} = RequestInfo {..}
    logResponseInfo resp = do
      let (status, headers, _) = responseToStream resp
          code = HTTP.statusCode status
          decodeHeader = bimap (decodeUtf8 . CI.original) decodeUtf8
      when (code >= 300) $
        logInfoIO "Error response" . show $
          ResponseInfo
            { statusCode = code,
              statusMessage = decodeUtf8 $ HTTP.statusMessage status,
              headers = decodeHeader <$> headers
            }
    loggedRespF resp = do
      logResponseInfo resp
      respF resp

withModifiedEnv :: (EnvR f -> Application) -> EnvR f -> Application
withModifiedEnv f env = \req resp -> do
  requestId <- getRequestId $ Wai.requestHeaders req
  modifiedEnv <- modifyEnvR requestId
  let app = f modifiedEnv
  app req resp
  where
    modifyEnvR requestId = return $ env {flowRuntime = L.updateLoggerContext (appendLogContext requestId) $ flowRuntime env}
    getRequestId headers = do
      let value = lookup "x-request-id" headers
      case value of
        Just val -> pure ("requestId-" <> T.pack (BS.unpack val))
        Nothing -> pure "randomRequestId-" <> show <$> nextRandom
