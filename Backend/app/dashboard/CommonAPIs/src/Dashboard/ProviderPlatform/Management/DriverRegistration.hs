{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Dashboard.ProviderPlatform.Management.DriverRegistration
  ( module Dashboard.ProviderPlatform.Management.DriverRegistration,
    module Reexport,
  )
where

import API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration as Reexport
import Dashboard.Common as Reexport
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.List (sortOn)
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Kernel.External.Notification.FCM.Types (FCMRecipientToken)
import Kernel.Prelude
import Kernel.Types.Predicate
import Kernel.Utils.Validation
import Servant

instance HideSecrets UploadDocumentReq where
  type ReqWithoutSecrets UploadDocumentReq = UploadDocumentTReq
  hideSecrets UploadDocumentReq {..} = UploadDocumentTReq {..}

instance FromHttpApiData [DocumentType] where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader bs = left T.pack . eitherDecode . BSL.fromStrict $ bs

instance ToHttpApiData [DocumentType] where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

-- auth  API ------------------------
-- ----------------------------------------

data AuthReq = AuthReq
  { mobileNumber :: Text,
    mobileCountryCode :: Text,
    name :: Maybe Text
  }
  deriving (Generic, FromJSON, ToSchema, ToJSON)

data AuthRes = AuthRes
  { authId :: Text,
    attempts :: Int
  }
  deriving (Generic, ToJSON, ToSchema, FromJSON)

-- verify  API ------------------------
-- ----------------------------------------

---------- Verify Login --------
data AuthVerifyReq = AuthVerifyReq
  { otp :: Text,
    deviceToken :: FCMRecipientToken
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

isDescOrder :: [Int] -> Bool
isDescOrder xs = xs == sortOn negate xs

validateTriggerReminderReq :: Validate TriggerReminderReq
validateTriggerReminderReq TriggerReminderReq {..} = do
  let mkMessage field = field <> " should have descending order"
  sequenceA_
    [ validateField "intervals" intervals $ InMaybe $ PredicateFunc mkMessage isDescOrder
    ]
