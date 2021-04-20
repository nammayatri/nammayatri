{-# LANGUAGE TypeApplications #-}

module Beckn.Utils.App
  ( handleLeft,
    handleShutdown,
    logRequestAndResponse,
    modifyEnvR,
  )
where

import Beckn.Types.App
import Beckn.Utils.Common
import Control.Concurrent.STM.TMVar
import qualified Data.Binary.Builder as B
import qualified Data.CaseInsensitive as CI
import Data.UUID.V4 (nextRandom)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified Network.HTTP.Types as HTTP
import Network.Wai
import System.Exit (ExitCode)
import System.Posix.Signals (Handler (Catch), installHandler, sigINT, sigTERM)

data ResponseInfo = ResponseInfo
  { statusCode :: Int,
    statusMessage :: Text,
    responseSucceeded :: Bool,
    responseBody :: LByteString,
    responseHeaders :: [(Text, Text)]
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
  logInfoIO "Request" $ show req
  f req loggedRespF
  where
    logInfoIO tag info = runFlowR flowRt appEnv $ logTagInfo tag info
    loggedRespF resp = do
      logInfoIO "Response" . show =<< toResponseInfo resp
      respF resp

toResponseInfo :: Response -> IO ResponseInfo
toResponseInfo resp = do
  let (status, headers, bodyWriter) = responseToStream resp
  body <- bodyWriter bodyToBytestring
  let code = HTTP.statusCode status
  return
    ResponseInfo
      { statusCode = code,
        statusMessage = decodeUtf8 $ HTTP.statusMessage status,
        responseSucceeded = code >= 200 && code < 300,
        responseBody = body,
        responseHeaders = decodeHeader <$> headers
      }
  where
    bodyToBytestring :: StreamingBody -> IO LByteString
    bodyToBytestring streamingBody = do
      content <- newIORef mempty
      streamingBody (\chunk -> modifyIORef' content (<> chunk)) pass
      -- Someday can do lazy IO above to spare some memory allocations
      B.toLazyByteString <$> readIORef content
    decodeHeader =
      bimap (decodeUtf8 . CI.original) decodeUtf8

modifyEnvR :: EnvR f -> IO (EnvR f)
modifyEnvR env = do
  uuid <- show <$> nextRandom
  return $ env {flowRuntime = L.updateLoggerContext (appendLogContext uuid) $ flowRuntime env}
