{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module AppAttestation.Canonical
  ( CanonicalRequest (..),
    canonicalText,
    requestDigest,
    keyAttestationDigest,
    androidRequestHash,
    sha256,
    encodeBase64Url,
    decodeBase64Lenient,
    stripBase64Padding,
  )
where

import qualified Crypto.Hash as Hash
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Kernel.Prelude

data CanonicalRequest
  = MobileAuthRequest {merchantId :: Text, mobileCountryCode :: Text, mobileNumber :: Text}
  | EmailAuthRequest {merchantId :: Text, email :: Text}
  | ResendRequest {authId :: Text}
  deriving (Show, Eq)

canonicalText :: CanonicalRequest -> Text -> Text
canonicalText request challenge =
  T.intercalate "|" $
    "v1" : case request of
      MobileAuthRequest {..} -> [merchantId, mobileCountryCode, mobileNumber, challenge]
      EmailAuthRequest {..} -> [merchantId, email, challenge]
      ResendRequest {..} -> ["resend", authId, challenge]

sha256 :: ByteString -> ByteString
sha256 = BA.convert . Hash.hashWith Hash.SHA256

requestDigest :: CanonicalRequest -> Text -> ByteString
requestDigest request = sha256 . TE.encodeUtf8 . canonicalText request

keyAttestationDigest :: Text -> ByteString
keyAttestationDigest = sha256 . TE.encodeUtf8

androidRequestHash :: CanonicalRequest -> Text -> Text
androidRequestHash request = encodeBase64Url . requestDigest request

encodeBase64Url :: ByteString -> Text
encodeBase64Url = stripBase64Padding . TE.decodeUtf8 . B64U.encode

stripBase64Padding :: Text -> Text
stripBase64Padding = T.dropWhileEnd (== '=')

decodeBase64Lenient :: Text -> Maybe ByteString
decodeBase64Lenient raw =
  let normalized = T.map toStandard . stripBase64Padding $ T.strip raw
      padded = normalized <> T.replicate ((4 - T.length normalized `mod` 4) `mod` 4) "="
   in if T.null normalized then Nothing else either (const Nothing) Just . B64.decode $ TE.encodeUtf8 padded
  where
    toStandard '-' = '+'
    toStandard '_' = '/'
    toStandard c = c
