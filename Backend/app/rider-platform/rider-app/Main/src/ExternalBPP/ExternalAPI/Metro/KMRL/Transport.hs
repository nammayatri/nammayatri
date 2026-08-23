module ExternalBPP.ExternalAPI.Metro.KMRL.Transport
  ( callKMRL,
    envelope,
    kmrlManager,
    callKMRLRaw,
    KMRLError (..),
  )
where

import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HMap
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Domain.Types.Extra.IntegratedBPPConfig (KMRLConfig (..))
import qualified ExternalBPP.ExternalAPI.Metro.KMRL.Crypto as Crypto
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Utils.Common
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Header as HTTP
import qualified Network.HTTP.Types.Status as HTTP
import Tools.Error
import Tools.HTTPManager (mutualTLSManagerKey, prepareMutualTLSHttpManager)

-- | KMRL wraps every request in Data/Risk/Links/Meta. Only Data carries anything; the Go
-- service leaves the other three as nil maps, which marshal to null.
envelope :: (A.ToJSON a) => a -> A.Value
envelope payload =
  A.object
    [ "Data" A..= payload,
      "Risk" A..= A.Null,
      "Links" A..= A.Null,
      "Meta" A..= A.Null
    ]

data KMRLError
  = KMRLCryptoError Text
  | KMRLTransportError Text
  | KMRLDecodeError Text
  | KMRLGatewayError Int Text
  deriving (Show)

instance IsBaseError KMRLError where
  toMessage = \case
    KMRLCryptoError m -> Just ("KMRL envelope error: " <> m)
    KMRLTransportError m -> Just ("KMRL transport error: " <> m)
    KMRLDecodeError m -> Just ("KMRL response could not be read: " <> m)
    KMRLGatewayError code m -> Just ("KMRL gateway returned " <> show code <> ": " <> m)

instance IsHTTPError KMRLError where
  toErrorCode = \case
    KMRLCryptoError _ -> "KMRL_CRYPTO_ERROR"
    KMRLTransportError _ -> "KMRL_TRANSPORT_ERROR"
    KMRLDecodeError _ -> "KMRL_DECODE_ERROR"
    KMRLGatewayError _ _ -> "KMRL_GATEWAY_ERROR"

instance IsAPIError KMRLError

instance Exception KMRLError

kmrlManager :: (MonadFlow m, EncFlow m r, MonadReader r m) => KMRLConfig -> m HTTP.Manager
kmrlManager config = do
  certPem <- decrypt config.clientCertPem
  settings <-
    prepareMutualTLSHttpManager "kmrl" 30000 (TE.encodeUtf8 certPem) (TE.encodeUtf8 <$> config.serverCaPem)
      >>= fromEitherM (\err -> InternalError $ "KMRL client certificate unusable: " <> err)
  managerSettings <-
    HMap.lookup (T.pack (mutualTLSManagerKey "kmrl")) settings
      & fromMaybeM (InternalError "KMRL mutual-TLS manager missing from the settings map")
  liftIO $ HTTP.newManager managerSettings

callKMRL ::
  (MonadFlow m, EncFlow m r, MonadReader r m, ToJSON req, FromJSON res) =>
  KMRLConfig ->
  HTTP.Manager ->
  BaseUrl ->
  Bool ->
  req ->
  m res
callKMRL config manager url withToken payload =
  callKMRLRaw config manager url withToken (BL.toStrict (A.encode payload))
    >>= \raw -> case A.eitherDecodeStrict raw of
      Right v -> pure v
      Left err -> throwError (KMRLDecodeError (T.pack err <> "; body was " <> TE.decodeUtf8With (\_ _ -> Just '?') raw))

callKMRLRaw ::
  (MonadFlow m, EncFlow m r, MonadReader r m) =>
  KMRLConfig ->
  HTTP.Manager ->
  BaseUrl ->
  Bool ->
  BS.ByteString ->
  m BS.ByteString
callKMRLRaw config manager url withToken payloadBytes = do
  (operatorPub, ourPriv) <- keysOf config
  signed <-
    liftIO (Crypto.encryptAndSign operatorPub ourPriv payloadBytes)
      >>= either (throwError . KMRLCryptoError) pure
  mbToken <- if withToken then Just <$> freshToken config manager else pure Nothing
  headers <- gatewayHeaders config mbToken
  responseBody <- post manager url headers signed
  Crypto.verifyAndDecrypt operatorPub ourPriv responseBody
    & either (throwError . KMRLCryptoError) pure

-- | Fetched per call, deliberately. KMRL AFCS tokens are SINGLE-USE: the gateway answers
-- 403 "Same token cannot be used multiple times" on any reuse, so caching one costs every
-- subsequent request. Go carries the same warning at @kochi_metro.go:275@.
freshToken :: (MonadFlow m, EncFlow m r, MonadReader r m) => KMRLConfig -> HTTP.Manager -> m Text
freshToken config manager = do
  authPassword <- decrypt config.kmrlAuthPassword
  let body =
        A.object
          [ "data"
              A..= A.object
                [ "authUserId" A..= config.kmrlAuthUserId,
                  "authPassword" A..= authPassword,
                  "channelId" A..= config.kmrlChannelId
                ]
          ]
  raw <- callKMRLRaw config manager config.tokenUrl False (BL.toStrict (A.encode body))
  case A.eitherDecodeStrict raw of
    Left err -> throwError (KMRLDecodeError ("token response: " <> T.pack err))
    Right (TokenResponse (TokenData (TokenPayload token))) -> pure token

newtype TokenResponse = TokenResponse {tokenResponseData :: TokenData}

instance FromJSON TokenResponse where
  parseJSON = A.withObject "TokenResponse" $ \o -> TokenResponse <$> o A..: "data"

newtype TokenData = TokenData {tokenDataData :: TokenPayload}

instance FromJSON TokenData where
  parseJSON = A.withObject "TokenData" $ \o -> TokenData <$> o A..: "data"

newtype TokenPayload = TokenPayload {tokenPayloadAccessToken :: Text}

instance FromJSON TokenPayload where
  parseJSON = A.withObject "TokenPayload" $ \o -> TokenPayload <$> o A..: "accessToken"

gatewayHeaders :: (MonadFlow m, EncFlow m r, MonadReader r m) => KMRLConfig -> Maybe Text -> m [HTTP.Header]
gatewayHeaders config mbToken = do
  clientSecret <- decrypt config.ibmClientSecret
  now <- getCurrentTime
  uuid <- generateGUID
  let epochMillis = show @Text (floor (utcTimeToPOSIXSeconds now * 1000) :: Integer)
      base =
        [ ("Content-Type", "text/plain"),
          ("X-IBM-Client-Id", TE.encodeUtf8 config.ibmClientId),
          ("X-IBM-Client-Secret", TE.encodeUtf8 clientSecret),
          ("x-fapi-channel-id", TE.encodeUtf8 config.fapiChannelId),
          ("x-fapi-uuid", TE.encodeUtf8 uuid),
          ("x-fapi-epoch-millis", TE.encodeUtf8 epochMillis)
        ]
  pure $ base <> maybe [] (\t -> [("Authorization", TE.encodeUtf8 t)]) mbToken

post :: (MonadFlow m) => HTTP.Manager -> BaseUrl -> [HTTP.Header] -> BS.ByteString -> m BS.ByteString
post manager url headers body = do
  initReq <- liftIO (HTTP.parseRequest (T.unpack (showBaseUrl url)))
  let req =
        initReq
          { HTTP.method = "POST",
            HTTP.requestHeaders = headers,
            HTTP.requestBody = HTTP.RequestBodyBS body
          }
  result <- liftIO (try @_ @SomeException (HTTP.httpLbs req manager))
  case result of
    Left err -> throwError (KMRLTransportError (show err))
    Right res -> do
      let status = HTTP.statusCode (HTTP.responseStatus res)
          payload = BL.toStrict (HTTP.responseBody res)
      if status >= 200 && status < 300
        then pure payload
        else throwError (KMRLGatewayError status (TE.decodeUtf8With (\_ _ -> Just '?') payload))

keysOf :: (MonadFlow m, EncFlow m r, MonadReader r m) => KMRLConfig -> m (Crypto.RSAPublicKey, Crypto.RSAPrivateKey)
keysOf config = do
  privPem <- decrypt config.signingPrivateKeyPem
  pub <-
    Crypto.publicKeyFromCertPem (TE.encodeUtf8 config.operatorPublicCertPem)
      & either (throwError . KMRLCryptoError) pure
  priv <-
    Crypto.privateKeyFromPem (TE.encodeUtf8 privPem)
      & either (throwError . KMRLCryptoError) pure
  pure (pub, priv)
