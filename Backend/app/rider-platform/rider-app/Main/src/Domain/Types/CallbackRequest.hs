{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.CallbackRequest where

import qualified Data.Aeson as A
import qualified Domain.Types.Merchant as DM
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Id

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
  deriving (Show, Read, Eq, Ord, Generic)

instance FromJSON CallbackRequestStatus where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON CallbackRequestStatus where
  toJSON = A.genericToJSON A.defaultOptions
