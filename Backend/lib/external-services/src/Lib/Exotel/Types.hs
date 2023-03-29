{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib.Exotel.Types where

import Control.Lens.TH
import Data.Aeson (encode)
import Data.Aeson.Casing
import Data.Aeson.TH
import Data.Aeson.Types
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import Data.Text.Conversions
import EulerHS.Prelude
import Kernel.Storage.Esqueleto (derivePersistField)
import Kernel.Utils.Dhall (FromDhall)
import Kernel.Utils.JSON
import Kernel.Utils.TH
import Servant.Client
import Web.FormUrlEncoded (ToForm, toForm)
import Web.Internal.HttpApiData

-- | Exotel API token
newtype ExotelApiToken = ExotelApiToken
  { getExotelApiToken :: Text
  }
  deriving newtype (Show, FromDhall)

deriveIdentifierInstances ''ExotelApiToken

-- | Exotel API token
newtype ExotelApiKey = ExotelApiKey
  { getExotelApiKey :: Text
  }
  deriving newtype (Show, FromDhall)

deriveIdentifierInstances ''ExotelApiKey

-- | Exotel sid
newtype ExotelAccountSID = ExotelAccountSID
  { getExotelAccountSID :: Text
  }
  deriving newtype (Show, FromDhall)

deriveIdentifierInstances ''ExotelAccountSID

-- | Exotel caller id
newtype ExotelCallerId = ExotelCallerId
  { getExotelCallerId :: Text
  }
  deriving newtype (Show, FromDhall)

deriveIdentifierInstances ''ExotelCallerId

-- | Exotel Service config
data ExotelCfg = ExotelCfg
  { apiKey :: ExotelApiKey,
    apiToken :: ExotelApiToken,
    sid :: ExotelAccountSID,
    callerId :: ExotelCallerId
  }
  deriving (Generic, FromDhall)

-- | Exotel call sid
-- an alpha-numeric unique identifier of the call
newtype ExotelCallSID = ExotelCallSID
  { getExotelCallSID :: Text
  }
  deriving (Show)

deriveIdentifierInstances ''ExotelCallSID

data ExotelAttachments = ExotelAttachments
  { callId :: Text,
    rideId :: Text
  }
  deriving (Generic, Eq, Show, FromJSON, ToJSON, ToSchema)

-- | Exotel response body
data ExotelRequest = ExotelRequest
  { -- String; The phone number that will be called first.
    -- Preferably in E.164 format. If not set, our system will try to
    -- match it with a country and make a call.
    -- If landline number, prefix it with STD code; Ex: 080XXXX2400
    from :: Text,
    -- String; Your customer's phone number.
    -- If landline number, prefix it with STD code; Ex: 080XXXX2400
    to :: Text,
    -- String; This is your ExoPhone/Exotel Virtual Number
    callerId :: Text,
    -- 	String; An HTTP POST request will be made to this URL depending
    --  on what events are subscribed using ‘StatusCallbackEvents’.
    --  Refer here for complete list of parameters which will be sent to your endpoint.
    statusCallbackUrl :: BaseUrl,
    -- Any application specific value like order id that will be passed back
    -- as a parameter in StatusCallback (only via 'terminal' StatusCallbackEvent)
    customField :: ExotelAttachments
  }
  deriving (Generic, Eq, Show)

instance ToForm ExotelRequest where
  toForm ExotelRequest {..} =
    [ ("From", toQueryParam from),
      ("To", toQueryParam to),
      ("CallerId", toQueryParam callerId),
      ("StatusCallback", T.pack $ showBaseUrl statusCallbackUrl),
      ("StatusCallbackEvents[0]", "terminal"),
      ("StatusCallbackContentType", "application/json"),
      ("CustomField", decodeUtf8 $ encode customField)
    ]

-- | Overall call status
data ExotelCallStatus
  = -- The call is ready and waiting in line before going out
    QUEUED
  | -- The call is ringing
    RINGING
  | -- The call was answered and is currently in progress
    IN_PROGRESS
  | -- The call was answered and has ended normally
    COMPLETED
  | -- The call could not be completed as dialled, most likely
    -- because the phone number was non-existent
    FAILED
  | -- The caller received a busy signal
    BUSY
  | -- The call ended without being answered
    NO_ANSWER
  | -- The call is canceled
    CANCELED
  | -- Invalid call status
    INVALID_STATUS
  deriving (Show, Eq, Read, Generic, ToSchema)

instance FromText ExotelCallStatus where
  fromText a =
    case a of
      "queued" -> QUEUED
      "ringing" -> RINGING
      "in-progress" -> IN_PROGRESS
      "completed" -> COMPLETED
      "busy" -> BUSY
      "no-answer" -> NO_ANSWER
      "failed" -> FAILED
      "canceled" -> CANCELED
      _ -> INVALID_STATUS

$(deriveJSON constructorsWithHyphensToLowerOptions ''ExotelCallStatus)

derivePersistField "ExotelCallStatus"

-- | Call direction
data ExotelDirection
  = -- Incoming call
    INBOUND
  | -- Outbound calls from Exotel dashboard
    OUTBOUND_DIAL
  | -- All other Outbound calls (API, campaign etc.)
    OUTBOUND_API
  deriving (Show, Eq, Read, Generic, ToSchema)

$(deriveJSON constructorsWithHyphensToLowerOptions ''ExotelDirection)

-- | Exotel response body
data ExotelResponseBody = ExotelResponseBody
  { -- string; an alpha-numeric unique identifier of the call
    exoSid :: ExotelCallSID,
    -- Time in format YYYY-MM-DD HH:mm:ss
    -- Date and time at which the user initiated the API
    exoDateCreated :: Text,
    -- Time in format YYYY-MM-DD HH:mm:ss
    -- Date and time at which the status of the call
    -- was last updated in our system
    exoDateUpdated :: Text,
    -- Exotel account SID
    exoAccountSid :: ExotelAccountSID,
    -- Your customer's phone number
    exoTo :: Text,
    -- The phone number that will be called first
    exoFrom :: Text,
    -- This is your ExoPhone/Exotel Virtual Number
    exoPhoneNumberSid :: ExotelCallerId,
    -- Overall call status
    exoStatus :: ExotelCallStatus,
    -- Time in format YYYY-MM-DD HH:mm:ss
    -- Date and time when the call request was initiated to the operator
    exoStartTime :: Text,
    -- Time in format YYYY-MM-DD HH:mm:ss
    -- Date and time when the call was completed
    exoEndTime :: Maybe Text,
    -- Call duration in seconds
    exoDuration :: Maybe Text,
    -- Double; If present, this will be the amount (in INR or USD) you have been charged for the call
    exoPrice :: Maybe Text,
    -- Call direction
    exoDirection :: ExotelDirection,
    -- human
    exoAnsweredBy :: Maybe Text,
    -- Uri is the path of the CallSid
    exoUri :: Text,
    -- Link to the call recording
    exoRecordingUrl :: Maybe BaseUrl
  }
  deriving (Eq, Show, Generic)

$(makeLenses ''ExotelResponseBody)

$(deriveFromJSON (aesonPrefix pascalCase) ''ExotelResponseBody)
$(deriveToJSON (aesonPrefix pascalCase) ''ExotelResponseBody)

-- | Exotel response on success
newtype ExotelResponse = ExotelResponse
  { exoCall :: ExotelResponseBody
  }
  deriving (Eq, Show, Generic)

$(makeLenses ''ExotelResponse)

$(deriveFromJSON (aesonPrefix pascalCase) ''ExotelResponse)
$(deriveToJSON (aesonPrefix pascalCase) ''ExotelResponse)

data ExotelCallCallback = ExotelCallCallback
  { -- string; an alpha-numeric unique identifier of the call
    callSid :: Text,
    eventType :: Text,
    dateCreated :: Text,
    dateUpdated :: Text,
    -- The phone number that was attempted to be called first.
    from :: Text,
    -- Your customer's phone number as set in the API request. This number will be connected after `From`.
    to :: Text,
    -- Overall call status, which could be one of: 'completed', 'failed', 'busy' or 'no-answer'
    status :: ExotelCallStatus,
    phoneNumberSid :: Text,
    startTime :: Text,
    endTime :: Text,
    direction :: ExotelDirection,
    recordingUrl :: BaseUrl,
    conversationDuration :: Int,
    legs :: [ExotelLeg],
    -- 	The value that was passed in the CustomField (attachments here) parameter of the API (if set during the request) will be populated here.
    customField :: ExotelAttachments
  }
  deriving (Generic, Show, ToJSON, ToSchema)

data ExotelLeg = ExotelLeg
  { onCallDuration :: Int,
    status :: ExotelCallStatus
  }
  deriving (Generic, Eq, Show, ToJSON, ToSchema)

instance FromJSON ExotelCallCallback where
  parseJSON v =
    withObject
      "ExotelCallCallback"
      ( \obj -> do
          callSid <- obj .: "CallSid"
          eventType <- obj .: "EventType"
          dateCreated <- obj .: "DateCreated"
          dateUpdated <- obj .: "DateUpdated"
          from <- obj .: "From"
          to <- obj .: "To"
          status <- obj .: "Status"
          phoneNumberSid <- obj .: "PhoneNumberSid"
          startTime <- obj .: "StartTime"
          endTime <- obj .: "EndTime"
          direction <- obj .: "Direction"
          recordingUrl <- obj .: "RecordingUrl"
          conversationDuration <- obj .: "ConversationDuration"
          customField <- obj .: "CustomField" >>= withText "CustomField" (parseCustomField v)
          legs <- obj .: "Legs"

          return (ExotelCallCallback {..})
      )
      v

parseCustomField :: Value -> Text -> Parser ExotelAttachments
parseCustomField v txt = do
  case decodeJSON txt of
    Just exoAttch -> return exoAttch
    Nothing -> typeMismatch "CustomField" v

instance FromJSON ExotelLeg where
  parseJSON = withObject "ExotelLeg" $ \obj -> do
    onCallDuration <- obj .: "OnCallDuration"
    status <- obj .: "Status"

    return (ExotelLeg {..})
