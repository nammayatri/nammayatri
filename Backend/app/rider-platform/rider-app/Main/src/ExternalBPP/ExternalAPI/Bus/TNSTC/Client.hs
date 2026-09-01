{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module ExternalBPP.ExternalAPI.Bus.TNSTC.Client
  ( callTnstc,
    tnstcSoapAction,
  )
where

import qualified Control.Exception as CE
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import Domain.Types.Extra.IntegratedBPPConfig (TNSTCConfig (..))
import qualified EulerHS.Language as L
import qualified EulerHS.Types as ET
import ExternalBPP.ExternalAPI.Bus.TNSTC.Error (TNSTCFault (..))
import Kernel.External.Encryption (decrypt)
import Kernel.Prelude
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Utils.Common
import Network.HTTP.Client (newManager)
import qualified Network.HTTP.Client as Http
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.SOAP (ResponseParser (..), invokeWS)
import Network.SOAP.Exception (SOAPFault (..), SOAPParsingError (..))
import Network.SOAP.Transport.HTTP (runQueryM)
import Servant.Client.Core (ClientError (..))
import Text.XML (def, renderText)
import Text.XML.Cursor (Cursor)
import Text.XML.Writer (ToXML, toXML)
import qualified Text.XML.Writer as XW
import Tools.Error

tnstcSoapAction :: TNSTCConfig -> Text -> String
tnstcSoapAction config opName =
  case config.soapActionPrefix of
    Nothing -> ""
    Just prefix -> T.unpack (prefix <> opName)

callTnstc ::
  ( ToXML b,
    MonadFlow m,
    EncFlow m r,
    Metrics.CoreMetrics m,
    HasField "requestId" r (Maybe Text)
  ) =>
  TNSTCConfig ->
  Text ->
  b ->
  (Cursor -> a) ->
  m a
callTnstc config opName body parseCursor = do
  mbPooled <- L.lookupHTTPManager (Just (ET.ManagerSelector "default"))
  manager <- case mbPooled of
    Just m -> pure m
    Nothing -> do
      logWarning "TNSTC: no pooled HTTP manager named 'default'; creating a local one"
      L.runIO $ newManager tlsManagerSettings
  mbRequestId <- asks (.requestId)
  logDebug $ "TNSTC request [" <> opName <> "]: " <> LT.toStrict (renderText def (XW.document "soapBody" (toXML body)))
  password <- decrypt config.password

  let transport =
        runQueryM
          manager
          (T.unpack (showBaseUrl config.networkHostUrl))
          (applyRequestOptions mbRequestId password)
          pure

  result <-
    measuringDuration (Metrics.addRequestLatency metricHost opName) $
      L.runIO $ do
        r <- CE.try @SomeException (invokeWS transport soapAction () body (CursorParser parseCursor))
        pure $ either (Left . ConnectionError) Right r

  case result of
    Right a -> pure a
    Left (ConnectionError err) -> mapSoapError err
    Left clientErr -> do
      logError $ "TNSTC client error [" <> opName <> "]: " <> show clientErr
      throwError $ ExternalAPICallError (Just "TNSTC_API") config.networkHostUrl clientErr
  where
    soapAction = tnstcSoapAction config opName
    metricHost = showBaseUrl config.networkHostUrl

    applyRequestOptions mbRequestId password req =
      pure
        . Http.applyBasicAuth (TE.encodeUtf8 config.username) (TE.encodeUtf8 password)
        $ req
          { Http.checkResponse = \_ _ -> pure (),
            Http.responseTimeout = Http.responseTimeoutMicro (30 * 1000000),
            Http.requestHeaders =
              Http.requestHeaders req
                <> maybe [] (\rid -> [("x-request-id", TE.encodeUtf8 rid)]) mbRequestId
          }

    mapSoapError err
      | Just (SOAPFault code msg detail) <- fromException err = do
        logError $
          "TNSTC fault [" <> opName <> "] code=" <> code
            <> " message="
            <> msg
            <> " detail="
            <> detail
        throwError $ TNSTCFault code msg
      | Just (SOAPParsingError msg) <- fromException err = do
        logError $ "TNSTC parse error [" <> opName <> "]: " <> T.pack msg
        throwError $ InternalError ("TNSTC response parse error: " <> T.pack msg)
      | otherwise = do
        logError $ "TNSTC transport error [" <> opName <> "]: " <> show err
        throwError $ ExternalAPICallError (Just "TNSTC_API") config.networkHostUrl (ConnectionError err)
