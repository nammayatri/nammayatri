{-# LANGUAGE TypeApplications #-}

module App where

import qualified App.Server as App
import Beckn.Constants.APIErrorCode
import qualified Beckn.External.FCM.Flow as FCM
import qualified Beckn.Types.App as App
import qualified Beckn.Utils.JWT as JWT
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vault.Lazy as V
import qualified EulerHS.Interpreters as R
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Runtime as R
import qualified EulerHS.Types as T
import qualified Network.HTTP.Types as H
import Network.Wai
import Network.Wai.Handler.Warp
  ( Settings,
    defaultSettings,
    run,
    runSettings,
    setOnExceptionResponse,
    setPort,
  )
import Servant
import Servant.Server
import Storage.DB.Config
import Storage.Redis.Config
import qualified System.Environment as SE

runTransporterBackendApp :: IO ()
runTransporterBackendApp = do
  port <- fromMaybe 8014 . (>>= readMaybe) <$> SE.lookupEnv "PORT"
  runTransporterBackendApp' port
    $ setOnExceptionResponse transporterExceptionResponse
    $ setPort port defaultSettings

runTransporterBackendApp' :: Int -> Settings -> IO ()
runTransporterBackendApp' port settings = do
  reqHeadersKey <- V.newKey
  let loggerCfg =
        T.defaultLoggerConfig
          { T._logToFile = True,
            T._logFilePath = "/tmp/beckn-transport.log",
            T._isAsync = True
          }
  R.withFlowRuntime (Just loggerCfg) $ \flowRt -> do
    putStrLn @String "Initializing DB Connections..."
    let prepare = prepareDBConnections
    try (R.runFlow flowRt prepare) >>= \case
      Left (e :: SomeException) -> putStrLn @String ("Exception thrown: " <> show e)
      Right _ -> do
        putStrLn @String "Initializing Redis Connections..."
        try (R.runFlow flowRt prepareRedisConnections) >>= \case
          Left (e :: SomeException) -> putStrLn @String ("Exception thrown: " <> show e)
          Right _ -> do
            putStrLn @String "Initializing Options..."
            try (R.runFlow flowRt prepareOptions) >>= \case
              Left (e :: SomeException) -> putStrLn @String ("Exception thrown: " <> show e)
              Right _ ->
                putStrLn @String ("Runtime created. Starting server at port " <> show port)
            runSettings settings $ App.run reqHeadersKey (App.Env flowRt)

transporterExceptionResponse :: SomeException -> Response
transporterExceptionResponse exception = do
  let anyException = fromException exception
  case anyException of
    Just ex ->
      responseLBS
        (H.Status (errHTTPCode ex) (BS.pack $ errReasonPhrase ex))
        ((H.hContentType, "application/json") : (errHeaders ex))
        (errBody ex)
    Nothing ->
      responseLBS
        H.internalServerError500
        [(H.hContentType, "application/json")]
        (Aeson.encode $ internalServerErr)

prepareOptions :: L.Flow ()
prepareOptions =
  -- FCM token ( options key = FCMTokenKey )
  createFCMTokenRefreshThread

createFCMTokenRefreshThread :: L.Flow ()
createFCMTokenRefreshThread = do
  fcmEnabled <- L.runIO $ SE.lookupEnv "FCM_JSON_PATH"
  case fcmEnabled of
    Nothing -> pure () --report error here if FCM is crucial
    Just _ -> L.forkFlow forkDesc $ do
      forever $ do
        token <- FCM.checkAndGetToken
        L.runIO $ case token of
          Left _ -> threadDelay $ 10 * 1000000 -- bad token? retry
          Right t -> do
            validityStatus <- JWT.isValid t
            threadDelay $
              1000000 * case validityStatus of
                JWT.JWTValid x ->
                  -- seconds before token expiration
                  if x > 300
                    then (fromInteger x) - 300
                    else 10
                _ -> 10 -- just a caution, it shuold be valid by this moment
      pure ()
  where
    forkDesc = "Forever loop that checks and refreshes FCM token"
