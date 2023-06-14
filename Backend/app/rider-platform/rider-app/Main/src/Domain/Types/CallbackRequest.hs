{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.CallbackRequest where

import Data.Aeson
import qualified Data.Bifunctor as BF
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import qualified Domain.Types.Merchant as DM
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Id
import Servant

data CallbackRequestE e = CallbackRequest
  { id :: Id CallbackRequest,
    merchantId :: Id DM.Merchant,
    customerName :: Maybe Text,
    customerPhone :: EncryptedHashedField e Text,
    customerMobileCountryCode :: Text,
    status :: CallbackRequestStatus,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic)

type CallbackRequest = CallbackRequestE 'AsEncrypted

data CallbackRequestStatus = PENDING | RESOLVED | CLOSED
  deriving (Show, Generic, Read, ToJSON, FromJSON, ToSchema, ToParamSchema, Eq)

instance FromHttpApiData CallbackRequestStatus where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = BF.first T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData CallbackRequestStatus where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

data CallbackRequestAPIEntity = CallbackRequestAPIEntity
  { id :: Id CallbackRequest,
    merchantId :: Id DM.Merchant,
    customerName :: Maybe Text,
    customerPhone :: Text,
    customerMobileCountryCode :: Text,
    status :: CallbackRequestStatus,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

mkCallbackRequestAPIEntity :: EncFlow m r => CallbackRequest -> m CallbackRequestAPIEntity
mkCallbackRequestAPIEntity CallbackRequest {..} = do
  customerPhone' <- decrypt customerPhone
  pure
    CallbackRequestAPIEntity
      { customerPhone = customerPhone',
        ..
      }
