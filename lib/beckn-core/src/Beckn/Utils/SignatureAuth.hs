module Beckn.Utils.SignatureAuth
  ( PublicKey,
    PrivateKey,
    SignatureAlgorithm (..),
    SignatureParams (..),
    SignaturePayload (..),
    KeyId (..),
    Hash,
    decode,
    encode,
    sign,
    verify,
    encodeKeyId,
    decodeKeyId,
    mkSignatureRealm,
    defaultHeaderFields,
    makeSignatureString,
    mkSignatureParams,
    generateKeyPair,
    becknSignatureHash,
    hashFromByteString,
    bodyHashHeader,
  )
where

import Beckn.Types.Base64
import Beckn.Types.Credentials
import Beckn.Types.Time (Seconds, getSeconds)
import qualified Crypto.Error as Crypto
import qualified Crypto.Hash as Hash
import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.CaseInsensitive as CI
import Data.List (lookup)
import qualified Data.Text as T
import qualified Data.Text as Text
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Time.Format
import EulerHS.Prelude
import Network.HTTP.Types (Header, HeaderName)
import Servant (FromHttpApiData (..), ToHttpApiData (..))
import Text.ParserCombinators.Parsec

-- | Implementation of HTTP Signature authorization based on
-- https://tools.ietf.org/id/draft-cavage-http-signatures-12.html

-- | Keys are bytestrings, aliased for readability
-- | Signatures are bytestrings, aliased for readability
type Signature = ByteString

-- | List of supported algorithms, this should grow over time
data SignatureAlgorithm
  = Hs2019
  | Ed25519
  deriving (Eq, Show)

type Hash = Hash.Digest Hash.Blake2b_512

hashFromByteString :: ByteString -> Maybe Hash
hashFromByteString = Hash.digestFromByteString

bodyHashHeader :: HeaderName
bodyHashHeader = "Beckn-Body-Hash"

becknSignatureHash :: ByteString -> Hash
becknSignatureHash = Hash.hash

defaultHeaderFields :: [Text]
defaultHeaderFields = ["(created)", "(expires)", "digest"]

encodeAlg :: SignatureAlgorithm -> Text
encodeAlg Hs2019 = "hs2019"
encodeAlg Ed25519 = "ed25519"

decodeAlg :: String -> Maybe SignatureAlgorithm
decodeAlg "hs2019" = Just Hs2019
decodeAlg "ed25519" = Just Ed25519
decodeAlg _ = Nothing

data KeyId = KeyId
  { subscriberId :: Text,
    uniqueKeyId :: Text,
    alg :: SignatureAlgorithm
  }
  deriving (Show, Eq, Generic)

encodeKeyId :: KeyId -> Text
encodeKeyId KeyId {..} = subscriberId <> "|" <> uniqueKeyId <> "|" <> encodeAlg alg

decodeKeyId :: Text -> Either String KeyId
decodeKeyId input =
  case Text.splitOn "|" input of
    [subscriberId, uniqueKeyId, rAlg] -> do
      alg <- maybeToRight "INVALID_ALG" . decodeAlg . Text.unpack $ rAlg
      pure KeyId {..}
    _ -> Left "INVALID_KEY_ID"

-- | Signature parameters as per the specification
data SignatureParams = SignatureParams
  { -- | The key Id that should be used to to generate/verify the signature
    keyId :: KeyId,
    -- | The signature algorithm to use
    algorithm :: SignatureAlgorithm,
    -- | Ordered list of headers to sign along with the request body
    headers :: [Text],
    -- | Optional signature creation date/time (as UNIX time)
    created :: Maybe POSIXTime,
    -- | Optional signature expiration date/time (as UNIX time)
    expires :: Maybe POSIXTime
  }
  deriving (Eq, Show, Generic)

mkSignatureParams :: Text -> Text -> POSIXTime -> Seconds -> SignatureAlgorithm -> SignatureParams
mkSignatureParams shortOrgId uniqueKeyId now validity alg =
  SignatureParams
    { keyId =
        KeyId
          { subscriberId = shortOrgId,
            uniqueKeyId = uniqueKeyId,
            alg = alg
          },
      algorithm = alg,
      headers = defaultHeaderFields,
      created = Just now,
      expires = Just $ now + (fromInteger . fromIntegral $ getSeconds validity)
    }

-- | Signature payload representation that carries signature and it's params
data SignaturePayload = SignaturePayload
  { signature :: Signature,
    params :: SignatureParams
  }
  deriving (Show, Eq, Generic)

instance ToHttpApiData SignaturePayload where
  toQueryParam = decodeUtf8 . encode

instance FromHttpApiData SignaturePayload where
  parseQueryParam = first show <$> decode . encodeUtf8

-- | Decode the contents of a signature header to a signature and corresponding params
decode :: ByteString -> Either String SignaturePayload
decode val = do
  values <-
    first show $
      parse signatureHeader "" val
  sig <-
    Base64.decode
      =<< (maybeToRight "no valid signature" . fmap fromString . lookup "signature") values
  key <-
    join . maybeToRight "no keyId" $ decodeKeyId . Text.pack <$> lookup "keyId" values
  alg <-
    maybeToRight "no algorithm" $
      decodeAlg =<< lookup "algorithm" values
  unless (checkAlg alg key) $ Left "Algorithm is invalid"
  hdrs <-
    maybeToRight "no headers" $
      T.splitOn " " . fromString <$> lookup "headers" values
  -- FIXME: these will silently fail
  let crt = fromInteger <$> (readMaybe =<< lookup "created" values)
  let expi = fromInteger <$> (readMaybe =<< lookup "expires" values)
  return $ SignaturePayload sig (SignatureParams key alg hdrs crt expi)
  where
    checkAlg algo KeyId {..} = algo == alg
    signatureHeader = do
      string "Signature" *> spaces
      keyValues `sepBy` (char ',' *> spaces)
    keyValues = do
      key <- many1 letter
      spaces *> char '=' *> spaces
      value <-
        choice
          [ many1 digit,
            quoted $ many1 $ satisfy (/= '"')
          ]
      return (key, value)
    quoted = between (char '"') (char '"')

-- | Encode a signature and corresponding params to a value that can be packed
-- into the appropriate HTTP header
encode :: SignaturePayload -> ByteString
encode SignaturePayload {..} =
  "Signature "
    <> "keyId=\""
    <> encodeUtf8 (encodeKeyId $ keyId params)
    <> "\","
    <> "algorithm=\""
    <> encodeUtf8 (encodeAlg $ algorithm params)
    <> "\","
    <> maybeTime "created" (created params)
    <> maybeTime "expires" (expires params)
    <> "headers=\""
    <> (encodeUtf8 . T.toLower . T.intercalate " " . headers) params
    <> "\","
    <> "signature=\""
    <> Base64.encode signature
    <> "\""
  where
    maybeTime h (Just v) = fromString $ h <> "=" <> formatTime defaultTimeLocale "%s" v <> ","
    maybeTime _ Nothing = ""

makeSignatureString :: SignatureParams -> Hash -> [Header] -> ByteString
makeSignatureString params bodyHash allHeaders =
  let signHeaders =
        catMaybes $
          fmap makeHeaderLine . findHeader <$> headers params
   in BS.intercalate "\n" signHeaders
  where
    makeHeaderLine (header, value) =
      (encodeUtf8 . T.toLower $ header) <> ": " <> value

    findHeader :: Text -> Maybe (Text, ByteString)
    findHeader h =
      let alg = algorithm params
       in (h,) . bsStrip
            <$> case h of
              "(created)" | alg == Hs2019 || alg == Ed25519 -> (show :: Int -> ByteString) . floor <$> created params
              "(expires)" | alg == Hs2019 || alg == Ed25519 -> (show :: Int -> ByteString) . floor <$> expires params
              "(created)" -> Nothing -- FIXME: this should error out
              "(expires)" -> Nothing -- FIXME: this should error out
              "digest" -> pure $ "BLAKE-512=" <> Base64.encode (BA.convert bodyHash)
              _ ->
                -- Find all instances of this header, concatenate values separated by a comma
                let ciHeader = CI.mk $ encodeUtf8 h
                 in bsToMaybe
                      . BS.intercalate ", "
                      $ snd <$> filter (\h' -> fst h' == ciHeader) allHeaders

    bsStrip = encodeUtf8 . T.strip . decodeUtf8
    bsToMaybe b = if null b then Nothing else Just b

-- | Sign a request given the key, parameters and request headers
sign :: PrivateKey -> SignatureParams -> Hash -> [Header] -> Maybe Signature
sign (Base64 key) params bodyHash allHeaders =
  let msg = makeSignatureString params bodyHash allHeaders
      sk = Ed25519.secretKey key
      pk = Ed25519.toPublic <$> sk
      signature = Ed25519.sign <$> sk <*> pk <*> pure msg
   in Crypto.maybeCryptoError $ BA.convert <$> signature

verify :: PublicKey -> SignatureParams -> Hash -> [Header] -> Signature -> Either Crypto.CryptoError Bool
verify (Base64 key) params bodyHash allHeaders signatureBs =
  let msg = makeSignatureString params bodyHash allHeaders
      pk = Ed25519.publicKey key
      signature = Ed25519.signature signatureBs
   in Crypto.eitherCryptoError $ Ed25519.verify <$> pk <*> pure msg <*> signature

mkSignatureRealm :: Text -> Text -> Header
mkSignatureRealm headerName host =
  ( CI.mk $ encodeUtf8 headerName,
    "Signature realm=\"" <> encodeUtf8 host <> "\",headers=\"" <> encodeUtf8 (unwords defaultHeaderFields) <> "\""
  )

-- Generates random public/private key pair for Ed25519 alg
generateKeyPair :: IO (PrivateKey, PublicKey)
generateKeyPair = do
  secretKey <- Ed25519.generateSecretKey
  let publicKey = Ed25519.toPublic secretKey
  pure (Base64 . BS.pack . BA.unpack $ secretKey, Base64 . BS.pack . BA.unpack $ publicKey)
