module ExternalBPP.ExternalAPI.Subway.CRIS.RouteFare where

import qualified Crypto.Cipher.AES as AES
import qualified Crypto.Cipher.Types as CT
import qualified Crypto.Error as CE
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
-- import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Domain.Types.Extra.IntegratedBPPConfig (CRISConfig)
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import ExternalBPP.ExternalAPI.Subway.CRIS.Auth (callCRISAPI)
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.App
import Kernel.Utils.Common
import Servant.API

data CRISFareRequest = CRISFareRequest
  { -- | mandatory
    tpAccountId :: Int,
    -- | mandatory
    mobileNo :: Int,
    -- | mandatory
    imeiNo :: Text,
    -- | mandatory (Already Shared)
    appCode :: Text,
    -- | mandatory
    appSession :: Text,
    -- | mandatory
    sourceCode :: Text,
    -- | "1 blank space"
    changeOver :: Text,
    -- | mandatory
    destCode :: Text,
    -- | mandatory
    ticketType :: Text,
    -- | mandatory
    sourceZone :: Text,
    -- | "1 blank space"
    via :: Text
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

data RouteFareRes = RouteFareRes
  { fare :: Double,
    distance :: Double,
    currency :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)

type RouteFareAPI =
  "t" :> "uts.cris.in" :> "tputs" :> "V" :> "get_route_fare_details"
    :> Header "Authorization" Text
    :> Header "Content-Type" Text
    :> ReqBody '[JSON] Text
    :> Post '[JSON] Text

routeFareAPI :: Proxy RouteFareAPI
routeFareAPI = Proxy

-- AES encryption key
-- encryptionKey :: BS.ByteString
-- encryptionKey = "2F4kflvIX4PRQYMxEcB9E69vA8R3tzqk"

-- Add PKCS7 padding function
pkcs7Pad :: ByteString -> ByteString
pkcs7Pad input =
  let blockSize = 16
      paddingLength = blockSize - (BS.length input `mod` blockSize)
      padding = BS.replicate paddingLength (fromIntegral paddingLength)
   in input <> padding

-- Function to encrypt the request payload
encryptPayload :: CRISFareRequest -> CRISConfig -> Text
encryptPayload req _config = do
  let key = "2F4kflvIX4PRQYMxEcB9E69vA8R3tzqk"
      keyBS = TE.encodeUtf8 key

      -- Manual JSON construction with exact field order
      jsonStr =
        "{\"tpAccountId\":" <> show (tpAccountId req)
          <> ",\"mobileNo\":"
          <> show (mobileNo req)
          <> ",\"imeiNo\":\""
          <> imeiNo req
          <> "\",\"appCode\":\""
          <> appCode req
          <> "\",\"appSession\":\""
          <> appSession req
          <> "\",\"sourceCode\":\""
          <> sourceCode req
          <> "\",\"changeOver\":\""
          <> changeOver req
          <> "\",\"destCode\":\""
          <> destCode req
          <> "\",\"ticketType\":\""
          <> ticketType req
          <> "\",\"sourceZone\":\""
          <> sourceZone req
          <> "\",\"via\":\""
          <> via req
          <> "\"}"

      plaintext = TE.encodeUtf8 jsonStr
      paddedPlaintext = pkcs7Pad plaintext

      cipher = case CT.cipherInit keyBS :: CE.CryptoFailable AES.AES256 of
        CE.CryptoPassed a -> a
        CE.CryptoFailed err -> error $ "Cipher initialization failed: " <> show err

      encrypted = CT.ecbEncrypt cipher paddedPlaintext

  decodeUtf8 $ B64.encode encrypted

-- Helper function to transform numbers to strings in JSON
transformNumbers :: Value -> Value
transformNumbers (Object obj) = Object $ fmap transformNumbers obj
transformNumbers (Array array) = Array $ fmap transformNumbers array
transformNumbers (Number n) = String $ T.pack $ show n -- Convert numbers to strings
transformNumbers other = other

-- For decryption (when needed):
decryptPayload :: Text -> CRISConfig -> Either String ByteString
decryptPayload encryptedText config = do
  let cipher = case CT.cipherInit (TE.encodeUtf8 config.clientKey) :: CE.CryptoFailable AES.AES256 of
        CE.CryptoPassed a -> a
        CE.CryptoFailed err -> error $ "Cipher initialization failed: " <> show err

  decodedData <- B64.decode $ encodeUtf8 encryptedText
  Right $ CT.ecbDecrypt cipher decodedData

getRouteFare ::
  ( CoreMetrics m,
    MonadFlow m,
    CacheFlow m r,
    EncFlow m r
  ) =>
  CRISConfig ->
  CRISFareRequest ->
  m Text
getRouteFare config request = do
  let jsonStr =
        "{\"tpAccountId\":" <> show (tpAccountId request)
          <> ",\"mobileNo\":"
          <> show (mobileNo request)
          <> ",\"imeiNo\":\""
          <> imeiNo request
          <> "\",\"appCode\":\""
          <> appCode request
          <> "\",\"appSession\":\""
          <> appSession request
          <> "\",\"sourceCode\":\""
          <> sourceCode request
          <> "\",\"changeOver\":\""
          <> changeOver request
          <> "\",\"destCode\":\""
          <> destCode request
          <> "\",\"ticketType\":\""
          <> ticketType request
          <> "\",\"sourceZone\":\""
          <> sourceZone request
          <> "\",\"via\":\""
          <> via request
          <> "\"}"

  logInfo $ "Plain JSON before encryption: " <> jsonStr

  let encryptedPayload = encryptPayload request config
  logInfo $ "Encrypted Payload: " <> encryptedPayload

  -- Create client function properly
  let eulerClientFn token =
        let client = ET.client routeFareAPI
         in client (Just $ "Bearer " <> token) (Just "application/json") encryptedPayload

  callCRISAPI config routeFareAPI eulerClientFn "getRouteFare"

sampleRequest :: CRISFareRequest
sampleRequest =
  CRISFareRequest
    { tpAccountId = 3700000,
      mobileNo = 9962013253,
      imeiNo = "ed409d8d764c04f7",
      appCode = "TPCODE",
      appSession = "1",
      sourceCode = "PER",
      changeOver = " ",
      destCode = "TBM",
      ticketType = "J",
      sourceZone = "SR",
      via = " "
    }
