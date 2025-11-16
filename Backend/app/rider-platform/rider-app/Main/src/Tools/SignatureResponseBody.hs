{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.SignatureResponseBody where

import qualified Crypto.Hash as Hash
import qualified Crypto.PubKey.RSA.PKCS15 as RSA.PKCS15
import qualified Data.Aeson as A
import qualified Data.ByteString.Base64 as B64
import Data.ByteString.Lazy as BSL
import Data.OpenApi (ToSchema)
import Data.Text as T
import qualified Data.Text.Encoding as TE
import Domain.Types.Merchant (Merchant)
import EulerHS.Prelude hiding (fromList, (.~))
import Kernel.Types.Base64
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import Web.JWT (readRsaSecret)

-- | Wraps response with signature in the body
-- The response will be transformed to: { data: <original>, signature: <sig>, timestamp: <time> }
data SignedResponseBody

-- | The envelope that wraps the original response with signature
data SignedResponse a = SignedResponse
  { responseData :: a,
    signature :: Text,
    timestamp :: UTCTime,
    merchantId :: ShortId Merchant
  }
  deriving (Generic, Show)

instance (ToSchema a) => ToSchema (SignedResponse a)

instance (ToJSON a) => ToJSON (SignedResponse a) where
  toJSON SignedResponse {..} =
    A.object
      [ "data" A..= responseData,
        "signature" A..= signature,
        "timestamp" A..= timestamp,
        "merchantId" A..= merchantId
      ]

instance (FromJSON a) => FromJSON (SignedResponse a) where
  parseJSON = A.withObject "SignedResponse" $ \o ->
    SignedResponse
      <$> o A..: "data"
      <*> o A..: "signature"
      <*> o A..: "timestamp"
      <*> o A..: "merchantId"

-- Config type to hold private key information
data SignatureResponseConfig = SignatureResponseConfig
  { merchantShortId :: ShortId Merchant,
    signingPrivateKey :: Base64 -- Base64 encoded RSA private key in PEM format
  }

-- | Helper function to wrap response with signature
-- This is what you call in your handler to sign the response
wrapWithSignature ::
  (MonadFlow m, ToJSON a) =>
  SignatureResponseConfig ->
  a ->
  m (SignedResponse a)
wrapWithSignature config originalResponse = do
  now <- getCurrentTime
  let dataJson = A.encode originalResponse
      signature = generateSignature config dataJson
  return
    SignedResponse
      { responseData = originalResponse,
        signature = signature,
        timestamp = now,
        merchantId = config.merchantShortId
      }

-- Generate RSA signature of the data
generateSignature :: SignatureResponseConfig -> BSL.ByteString -> Text
generateSignature config body =
  let Base64 keyText = config.signingPrivateKey
      keyBytes = B64.decodeLenient keyText
   in case readRsaSecret keyBytes of
        Just privateKey ->
          case RSA.PKCS15.sign Nothing (Just Hash.SHA256) privateKey (BSL.toStrict body) of
            Left _ -> ""
            Right signature -> TE.decodeUtf8 $ B64.encode signature
        Nothing -> ""
