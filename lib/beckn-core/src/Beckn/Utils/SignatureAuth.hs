module Beckn.Utils.SignatureAuth
  ( PublicKey,
    PrivateKey,
    SignatureAlgorithm (..),
    SignatureParams (..),
    decode,
    encode,
    sign,
    verify,
  )
where

import qualified Crypto.Error as Crypto
import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.CaseInsensitive as CI
import Data.List (lookup)
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime)
import Data.Time.Format
import EulerHS.Prelude
import Network.HTTP.Types (Header, Method)
import Text.ParserCombinators.Parsec

-- | Implementation of HTTP Signature authorization based on
-- https://tools.ietf.org/id/draft-cavage-http-signatures-12.html

-- | Keys are bytestrings, aliased for readability
type PrivateKey = ByteString

type PublicKey = ByteString

-- | Signatures are bytestrings, aliased for readability
type Signature = ByteString

-- | List of supported algorithms, this should grow over time
data SignatureAlgorithm
  = Hs2019
  deriving (Eq, Show)

encodeAlg :: SignatureAlgorithm -> ByteString
encodeAlg Hs2019 = "hs2019"

decodeAlg :: String -> Maybe SignatureAlgorithm
decodeAlg "hs2019" = Just Hs2019
decodeAlg _ = Nothing

-- | Signature parameters as per the specification
data SignatureParams = SignatureParams
  { -- | The key ID that should be used to to generate/verify the signature
    keyId :: Text,
    -- | The signature algorithm to use
    algorithm :: SignatureAlgorithm,
    -- | Ordered list of headers to sign along with the request body
    headers :: [Text],
    -- | Optional signature creation date/time (as UNIX time)
    created :: Maybe NominalDiffTime,
    -- | Optional signature expiration date/time (as UNIX time)
    expires :: Maybe NominalDiffTime
  }
  deriving (Eq, Show)

-- | Decode the contents of a signature header to a signature and corresponding params
decode :: ByteString -> Either String (Signature, SignatureParams)
decode val = do
  values <-
    first show $
      parse signatureHeader "" val
  sig <-
    Base64.decode
      =<< (maybeToRight "no valid signature" . fmap fromString . lookup "signature") values
  key <-
    maybeToRight "no keyId" $
      fromString <$> lookup "keyId" values
  alg <-
    maybeToRight "no algorithm" $
      decodeAlg =<< lookup "algorithm" values
  hdrs <-
    maybeToRight "no headers" $
      T.splitOn " " . fromString <$> lookup "headers" values
  -- FIXME: these will silently fail
  let crt = fromInteger <$> (readMaybe =<< lookup "created" values)
  let expi = fromInteger <$> (readMaybe =<< lookup "expires" values)
  return (sig, SignatureParams key alg hdrs crt expi)
  where
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
encode :: Signature -> SignatureParams -> ByteString
encode val params =
  "Signature "
    <> "keyId=\""
    <> encodeUtf8 (keyId params)
    <> "\","
    <> "algorithm=\""
    <> encodeAlg (algorithm params)
    <> "\","
    <> maybeTime "created" (created params)
    <> maybeTime "expires" (expires params)
    <> "headers=\""
    <> (encodeUtf8 . T.toLower . T.intercalate " " . headers) params
    <> "\","
    <> "signature=\""
    <> Base64.encode val
    <> "\""
  where
    maybeTime h (Just v) = fromString $ h <> "=" <> formatTime defaultTimeLocale "%s" v <> ","
    maybeTime _ Nothing = ""

makeSignatureString :: SignatureParams -> Method -> ByteString -> [Header] -> ByteString
makeSignatureString params method path allHeaders =
  let signHeaders =
        catMaybes $
          fmap makeHeaderLine . findHeader <$> headers params
   in BS.intercalate "\n" signHeaders
  where
    makeHeaderLine (header, value) =
      (encodeUtf8 . T.toLower $ header) <> ": " <> value

    findHeader :: Text -> Maybe (Text, ByteString)
    findHeader h =
      (h,) . bsStrip
        <$> case h of
          "(request-target)" -> Just $ bsToLower method <> " " <> path
          "(created)" | algorithm params == Hs2019 -> show <$> created params
          "(expires)" | algorithm params == Hs2019 -> show <$> expires params
          "(created)" -> Nothing -- FIXME: this should error out
          "(expires)" -> Nothing -- FIXME: this should error out
          _ ->
            -- Find all instances of this header, concatenate values separated by a comma
            let ciHeader = CI.mk $ encodeUtf8 h
             in bsToMaybe
                  . BS.intercalate ", "
                  $ snd <$> filter (\h' -> fst h' == ciHeader) allHeaders

    bsToLower = encodeUtf8 . T.toLower . decodeUtf8
    bsStrip = encodeUtf8 . T.strip . decodeUtf8
    bsToMaybe b = if null b then Nothing else Just b

-- | Sign a request given the key, parameters and request headers
sign :: PrivateKey -> SignatureParams -> Method -> ByteString -> [Header] -> Maybe Signature
sign key params method path allHeaders =
  let msg = makeSignatureString params method path allHeaders
      sk = Ed25519.secretKey key
      pk = Ed25519.toPublic <$> sk
      signature = Ed25519.sign <$> sk <*> pk <*> pure msg
   in Crypto.maybeCryptoError $ BA.convert <$> signature

verify :: PublicKey -> SignatureParams -> Method -> ByteString -> [Header] -> Signature -> Bool
verify key params method path allHeaders signatureBs =
  let msg = makeSignatureString params method path allHeaders
      pk = Ed25519.publicKey key
      signature = Ed25519.signature signatureBs
   in case Ed25519.verify <$> pk <*> pure msg <*> signature of
        Crypto.CryptoPassed True -> True
        _ -> False
