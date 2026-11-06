{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
-- wai 3.2.3 has no setter for the request body other than the deprecated 'requestBody' field.
{-# OPTIONS_GHC -Wno-warnings-deprecations #-}

-- | Per-request payload encryption for the driver app, layered on top of HTTPS.
--
-- Every request carries a fresh X25519 public key from the app. Both sides compute
-- SECRET-A = ECDH(temp key, server key), then:
--
--   request key  = HKDF(SECRET-A, "request")
--   response key = HKDF(SECRET-A, "response" <> R)   -- R: 32 random bytes per response
--
-- Nothing is stored per driver or per session; the only long-lived secret is the
-- server private key, read once at startup.
module Tools.PayloadEncryption
  ( PayloadEncryptionConfig (..),
    loadPayloadEncryptionConfig,
    payloadEncryptionMiddleware,
  )
where

import qualified Crypto.Cipher.AES as AES
import qualified Crypto.Cipher.Types as Cipher
import Crypto.Error (CryptoFailable, maybeCryptoError)
import qualified Crypto.Hash.Algorithms as Hash
import qualified Crypto.KDF.HKDF as HKDF
import qualified Crypto.PubKey.Curve25519 as X25519
import Crypto.Random (getRandomBytes)
import qualified Data.Aeson as A
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as BSL
import Data.List (lookup)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import EulerHS.Prelude
import Network.HTTP.Types (HeaderName, Method, Status, hContentLength, hContentType, methodGet, methodHead, status400, statusCode)
import Network.Wai
import Network.Wai.Internal (Request (..))
import System.Environment (lookupEnv)

-- | The server's X25519 private keys by key id. This is the only setting: whether a request is
-- encrypted is decided per request by the app's @x-enc-version@ header, not by server config.
newtype PayloadEncryptionConfig = PayloadEncryptionConfig
  { privateKeys :: Map.Map ByteString X25519.SecretKey
  }

-- | Reads the keys from @DRIVER_PAYLOAD_ENC_PRIVATE_KEYS@ ("v1:<base64 32-byte key>,v2:...").
-- With no keys, plain requests work as before and encrypted ones are rejected as an unknown key id.
loadPayloadEncryptionConfig :: IO PayloadEncryptionConfig
loadPayloadEncryptionConfig =
  lookupEnv "DRIVER_PAYLOAD_ENC_PRIVATE_KEYS" >>= \case
    Nothing -> pure $ PayloadEncryptionConfig Map.empty
    Just rawKeys -> either (fail . T.unpack) (pure . PayloadEncryptionConfig) $ parsePrivateKeys (T.pack rawKeys)

parsePrivateKeys :: Text -> Either Text (Map.Map ByteString X25519.SecretKey)
parsePrivateKeys raw = do
  entries <- mapM parseEntry . filter (not . T.null) . map T.strip $ T.splitOn "," raw
  when (null entries) $ Left "DRIVER_PAYLOAD_ENC_PRIVATE_KEYS has no keys"
  pure $ Map.fromList entries
  where
    parseEntry entry = case T.splitOn ":" entry of
      [kid, encoded] | not (T.null kid) -> do
        keyBytes <- first (const $ "Private key " <> kid <> " is not valid base64") $ B64.decode (TE.encodeUtf8 encoded)
        secretKey <- maybeCryptoError (X25519.secretKey keyBytes) `orFail` ("Private key " <> kid <> " is not a 32-byte X25519 key")
        pure (TE.encodeUtf8 kid, secretKey)
      _ -> Left "Expected entries of the form <kid>:<base64 key>"

-- | App versions with encryption send @x-enc-version: v1@; older versions send nothing and are
-- passed through untouched, as are all other callers (Beckn, webhooks, dashboards).
--
-- All header checks and the key agreement run before the body is read, so a request with bad
-- encryption headers is rejected without buffering its body.
payloadEncryptionMiddleware :: PayloadEncryptionConfig -> Middleware
payloadEncryptionMiddleware cfg app req respond =
  case lookup versionHeader (requestHeaders req) of
    Nothing -> app req respond
    Just version
      | version /= supportedVersion -> respond $ encryptionError "PAYLOAD_ENCRYPTION_UNSUPPORTED_VERSION"
      | otherwise -> do
        now <- floor <$> getPOSIXTime
        case verifyHeaders cfg now req of
          Left errorCode -> respond $ encryptionError errorCode
          Right sealContext -> do
            sealedBody <- BSL.toStrict <$> strictRequestBody req
            case openBody sealContext req sealedBody of
              Left errorCode -> respond $ encryptionError errorCode
              Right plaintext -> do
                plainReq <- withPlainBody sealContext plaintext req
                app plainReq (sealResponse sealContext req >=> respond)

-- | What both directions of one request need: the key derived from SECRET-A and the request's AAD.
data SealContext = SealContext
  { prk :: HKDF.PRK Hash.SHA256,
    requestAad :: ByteString,
    contentType :: ByteString
  }

verifyHeaders :: PayloadEncryptionConfig -> Integer -> Request -> Either Text SealContext
verifyHeaders cfg now req = do
  kid <- header keyIdHeader `orFail` "PAYLOAD_ENCRYPTION_KEY_ID_MISSING"
  serverKey <- Map.lookup kid cfg.privateKeys `orFail` "PAYLOAD_ENCRYPTION_UNKNOWN_KEY_ID"
  rawTimestamp <- header timestampHeader `orFail` "PAYLOAD_ENCRYPTION_TIMESTAMP_MISSING"
  timestamp <- readMaybe (decodeUtf8 rawTimestamp :: String) `orFail` "PAYLOAD_ENCRYPTION_INVALID_TIMESTAMP"
  when (abs (now - timestamp) > maxClockSkewSeconds) $ Left "PAYLOAD_ENCRYPTION_STALE_REQUEST"
  encodedClientKey <- header clientKeyHeader `orFail` "PAYLOAD_ENCRYPTION_CLIENT_KEY_MISSING"
  clientKeyBytes <- first (const "PAYLOAD_ENCRYPTION_INVALID_CLIENT_KEY") $ B64.decode encodedClientKey
  clientKey <- maybeCryptoError (X25519.publicKey clientKeyBytes) `orFail` "PAYLOAD_ENCRYPTION_INVALID_CLIENT_KEY"
  let secretA = BA.convert (X25519.dh clientKey serverKey) :: ByteString
  -- A low-order client key yields an all-zero secret (RFC 7748 §6.1); never derive keys from it.
  when (BS.all (== 0) secretA) $ Left "PAYLOAD_ENCRYPTION_INVALID_CLIENT_KEY"
  let prk = HKDF.extract hkdfSalt secretA
      contentType = fromMaybe defaultContentType (header contentTypeHeader)
      requestAad = joinAad [protocolVersion, requestMethod req, rawPathInfo req <> rawQueryString req, rawTimestamp, contentType]
  pure SealContext {..}
  where
    header name = lookup name (requestHeaders req)

-- | GET/HEAD cannot carry the sealed tag; the response is still sealed to the client's key and
-- bound to this request's AAD, so a redirected or replayed GET is unreadable to anyone else.
openBody :: SealContext -> Request -> ByteString -> Either Text ByteString
openBody sealContext req sealedBody
  | BS.null sealedBody && requestMethod req `elem` [methodGet, methodHead] = Right BS.empty
  | otherwise =
    aesGcmOpen (HKDF.expand sealContext.prk requestInfo keyMaterialLength) sealContext.requestAad sealedBody
      `orFail` "PAYLOAD_DECRYPTION_FAILED"

-- | Hands the decrypted body to the inner application as if the app had sent it in plain. The
-- @x-enc*@ headers are dropped too: the request is no longer encrypted, and the cross-cloud proxy
-- forwards it as-is, so a remote driver app would otherwise try to decrypt a plain body.
--
-- Built with the 'Request' constructor rather than record-update syntax: treefmt's ormolu
-- writes an update as @req{...}@, which RecordDotPreprocessor rewrites into a 'setField'
-- that Wai's record has no instance for.
withPlainBody :: SealContext -> ByteString -> Request -> IO Request
withPlainBody sealContext plaintext Request {..} = do
  remaining <- newMVar plaintext
  pure
    Request
      { requestBody = tryTakeMVar remaining <&> fromMaybe BS.empty,
        requestHeaders = (hContentType, sealContext.contentType) : filter (not . isReplacedRequestHeader . fst) requestHeaders,
        requestBodyLength = KnownLength (fromIntegral $ BS.length plaintext),
        ..
      }

-- | Responses that carry no body on the wire (HEAD, 1xx, 204, 304) would lose the GCM tag, so
-- they go back unsealed; clients treat a response without @x-enc-resp-nonce@ as plain.
sealResponse :: SealContext -> Request -> Response -> IO Response
sealResponse sealContext req response
  | not (canCarryBody (requestMethod req) (responseStatus response)) = pure response
  | otherwise = do
    let (status, headers, withBody) = responseToStream response
    body <- withBody collectStreamingBody
    responseNonce <- getRandomBytes responseNonceLength :: IO ByteString
    let keyMaterial = HKDF.expand sealContext.prk (responseInfo <> responseNonce) keyMaterialLength
        responseAad = joinAad [sealContext.requestAad, responseInfo, show (statusCode status)]
        originalContentType = fromMaybe defaultContentType (lookup hContentType headers)
    pure $ case aesGcmSeal keyMaterial responseAad body of
      Nothing -> encryptionError "PAYLOAD_ENCRYPTION_FAILED"
      Just sealedBody ->
        responseLBS
          status
          ( (hContentType, "application/octet-stream") :
            (responseNonceHeader, B64.encode responseNonce) :
            (contentTypeHeader, originalContentType) :
            filter (not . isBodyHeader . fst) headers
          )
          (BSL.fromStrict sealedBody)

canCarryBody :: Method -> Status -> Bool
canCarryBody method status =
  method /= methodHead && code >= 200 && code `notElem` [204, 304]
  where
    code = statusCode status

collectStreamingBody :: StreamingBody -> IO ByteString
collectStreamingBody streamingBody = do
  chunks <- newIORef mempty
  streamingBody (\chunk -> modifyIORef' chunks (<> chunk)) (pure ())
  BSL.toStrict . Builder.toLazyByteString <$> readIORef chunks

-- | Key material is a 32-byte AES-256 key followed by a 12-byte GCM nonce. Each key encrypts
-- exactly one message, so the derived nonce is never reused.
aesGcmSeal :: ByteString -> ByteString -> ByteString -> Maybe ByteString
aesGcmSeal keyMaterial aad plaintext = do
  aead <- initAead keyMaterial
  let (Cipher.AuthTag tag, ciphertext) = Cipher.aeadSimpleEncrypt aead aad plaintext gcmTagLength
  pure $ ciphertext <> BA.convert tag

aesGcmOpen :: ByteString -> ByteString -> ByteString -> Maybe ByteString
aesGcmOpen keyMaterial aad sealed = do
  guard (BS.length sealed >= gcmTagLength)
  aead <- initAead keyMaterial
  let (ciphertext, tag) = BS.splitAt (BS.length sealed - gcmTagLength) sealed
  Cipher.aeadSimpleDecrypt aead aad ciphertext (Cipher.AuthTag $ BA.convert tag)

initAead :: ByteString -> Maybe (Cipher.AEAD AES.AES256)
initAead keyMaterial = do
  let (key, nonce) = BS.splitAt aesKeyLength keyMaterial
  cipher <- maybeCryptoError (Cipher.cipherInit key :: CryptoFailable AES.AES256)
  maybeCryptoError $ Cipher.aeadInit Cipher.AEAD_GCM cipher nonce

encryptionError :: Text -> Response
encryptionError errorCode =
  responseLBS status400 [(hContentType, "application/json")] $
    A.encode $ A.object ["errorCode" A..= errorCode, "errorMessage" A..= errorCode]

-- | NUL-separated so that no two different field lists produce the same AAD.
joinAad :: [ByteString] -> ByteString
joinAad = BS.intercalate "\0"

isBodyHeader :: HeaderName -> Bool
isBodyHeader name = name `elem` [hContentType, hContentLength, "Transfer-Encoding"]

isReplacedRequestHeader :: HeaderName -> Bool
isReplacedRequestHeader name =
  isBodyHeader name || name `elem` [versionHeader, keyIdHeader, clientKeyHeader, timestampHeader, contentTypeHeader]

orFail :: Maybe a -> Text -> Either Text a
orFail value errorCode = maybe (Left errorCode) Right value

versionHeader, keyIdHeader, clientKeyHeader, timestampHeader, contentTypeHeader, responseNonceHeader :: HeaderName
versionHeader = "x-enc-version"
keyIdHeader = "x-enc-kid"
clientKeyHeader = "x-enc"
timestampHeader = "x-enc-ts"
contentTypeHeader = "x-enc-content-type"
responseNonceHeader = "x-enc-resp-nonce"

supportedVersion, protocolVersion, hkdfSalt, requestInfo, responseInfo, defaultContentType :: ByteString
supportedVersion = "v1"
protocolVersion = "ny-enc-v1"
hkdfSalt = protocolVersion
requestInfo = "request"
responseInfo = "response"
defaultContentType = "application/json"

aesKeyLength, keyMaterialLength, gcmTagLength, responseNonceLength :: Int
aesKeyLength = 32
keyMaterialLength = aesKeyLength + 12
gcmTagLength = 16
responseNonceLength = 32

-- | How far the request's timestamp may drift from server time before it is treated as a replay.
maxClockSkewSeconds :: Integer
maxClockSkewSeconds = 120
