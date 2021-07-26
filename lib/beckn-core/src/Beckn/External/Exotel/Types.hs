{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Beckn.External.Exotel.Types where

import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.JSON
import Beckn.Utils.TH
import Control.Lens.TH
import Data.Aeson.Casing
import Data.Aeson.TH
import EulerHS.Prelude
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

-- | Exotel response body
data ExotelRequest = ExotelRequest
  { -- String; The phone number that will be called first.
    -- Preferably in E.164 format. If not set, our system will try to
    -- match it with a country and make a call.
    -- If landline number, prefix it with STD code; Ex: 080XXXX2400
    from :: !Text,
    -- String; Your customer's phone number.
    -- If landline number, prefix it with STD code; Ex: 080XXXX2400
    to :: !Text,
    -- String; This is your ExoPhone/Exotel Virtual Number
    callerId :: !Text
  }
  deriving (Eq, Show)

instance ToForm ExotelRequest where
  toForm ExotelRequest {..} =
    [ ("From", toQueryParam from),
      ("To", toQueryParam to),
      ("CallerId", toQueryParam callerId)
    ]

-- | Overall call status
data ExotelCallStatus
  = -- The call is ready and waiting in line before going out
    QUEUED
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
  deriving (Show, Eq, Read, Generic)

$(deriveJSON constructorsWithHyphensToLowerOptions ''ExotelCallStatus)

-- | Call direction
data ExotelDirection
  = -- Incoming call
    INBOUND
  | -- Outbound calls from Exotel dashboard
    OUTBOUND_DIAL
  | -- All other Outbound calls (API, campaign etc.)
    OUTBOUND_API
  deriving (Show, Eq, Read, Generic)

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
    exoEndTime :: Text,
    -- Call duration in seconds
    exoDuration :: Text,
    -- Double; If present, this will be the amount (in INR or USD) you have been charged for the call
    exoPrice :: Text,
    -- Call direction
    exoDirection :: ExotelDirection,
    -- human
    exoAnsweredBy :: Text,
    -- Uri is the path of the CallSid
    exoUri :: Text,
    -- Link to the call recording
    exoRecordingUrl :: Text
  }
  deriving (Eq, Show)

$(makeLenses ''ExotelResponseBody)

$(deriveFromJSON (aesonPrefix pascalCase) ''ExotelResponseBody)

-- | Exotel response on success
newtype ExotelResponse = ExotelResponse
  { exoCall :: ExotelResponseBody
  }
  deriving (Eq, Show)

$(makeLenses ''ExotelResponse)

$(deriveFromJSON (aesonPrefix pascalCase) ''ExotelResponse)
