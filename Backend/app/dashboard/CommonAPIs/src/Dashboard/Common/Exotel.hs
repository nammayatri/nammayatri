{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}

module Dashboard.Common.Exotel
  ( module Dashboard.Common.Exotel,
    module Reexport,
  )
where

import Dashboard.Common as Reexport
import Data.Aeson
import qualified Data.Aeson.Key as AesonKey (fromText)
import Data.Aeson.Types
import Data.OpenApi
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.APISuccess
import Kernel.Utils.JSON (constructorsWithSnakeCase)
import Servant hiding (Summary)

-- we need to save endpoint transactions only for POST, PUT, DELETE APIs
data ExotelEndpoint
  = ExotelHeartbeatEndpoint
  deriving (Show, Read, ToJSON, FromJSON, Generic, Eq, Ord)

derivePersistField "ExotelEndpoint"

---------------------------------------------------------
-- exotel heartbeat --------------------------------------

type ExotelHeartbeatAPI =
  "heartbeat"
    :> ReqBody '[JSON] ExotelHeartbeatReq
    :> Post '[JSON] APISuccess

data ExotelHeartbeatReq = ExotelHeartbeatReq
  { timestamp :: UTCTime,
    statusType :: StatusType,
    incomingAffected :: [PhoneNumber],
    outgoingAffected :: [PhoneNumber]
  }
  deriving stock (Show, Generic)

-- ToSchema instance is wrong, but anyway this api shouldn't be used by dashboard ops
instance ToSchema ExotelHeartbeatReq where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions constructorsWithSnakeCase

data StatusType = OK | WARNING | CRITICAL | PAYLOAD_TOO_LARGE
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PhoneNumber = PhoneNumber
  { phoneNumberSid :: Text,
    phoneNumber :: Text
  }
  deriving stock (Show, Generic)

instance ToSchema PhoneNumber where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions constructorsWithSnakeCase

instance FromJSON ExotelHeartbeatReq where
  parseJSON (Object obj) = do
    timestamp <- obj .: "timestamp"
    statusType <- obj .: "status_type"
    incomingAffectedSids <- fromMaybe [] <$> (obj .:? "incoming_affected")
    outgoingAffectedSids <- fromMaybe [] <$> (obj .:? "outgoing_affected")
    dataObj <- obj .: "data"
    incomingAffected <- for incomingAffectedSids $ \phoneNumberSid -> do
      phoneNumber <- (dataObj .: AesonKey.fromText phoneNumberSid) >>= (.: "phone_number")
      pure PhoneNumber {..}
    outgoingAffected <- for outgoingAffectedSids $ \phoneNumberSid -> do
      phoneNumber <- (dataObj .: AesonKey.fromText phoneNumberSid) >>= (.: "phone_number")
      pure PhoneNumber {..}
    pure ExotelHeartbeatReq {..}
  parseJSON wrongVal = typeMismatch "Object ExotelHeartbeatReq" wrongVal

instance ToJSON ExotelHeartbeatReq where
  toJSON ExotelHeartbeatReq {..} = do
    let incomingAffectedSids = incomingAffected <&> (.phoneNumberSid)
    let outgoingAffectedSids = outgoingAffected <&> (.phoneNumberSid)
    let affectedPhones = incomingAffected <> filter (\phone -> phone.phoneNumberSid `notElem` incomingAffectedSids) outgoingAffected
    object
      [ "timestamp" .= timestamp,
        "status_type" .= statusType,
        "incoming_affected" .= if null incomingAffectedSids then Nothing else Just incomingAffectedSids,
        "outgoing_affected" .= if null outgoingAffectedSids then Nothing else Just outgoingAffectedSids,
        "data" .= object (mkDataListItem <$> affectedPhones)
      ]
    where
      mkDataListItem PhoneNumber {..} = AesonKey.fromText phoneNumberSid .= object ["phone_number" .= phoneNumber]

instance HideSecrets ExotelHeartbeatReq where
  hideSecrets = identity
