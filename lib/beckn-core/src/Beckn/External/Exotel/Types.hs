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

import Beckn.Utils.TH
import Control.Lens.TH
import Data.Aeson
import Data.Aeson.Casing
import Data.Aeson.TH
import EulerHS.Prelude
import Web.FormUrlEncoded (ToForm, toForm)
import Web.Internal.HttpApiData

-- | Exotel API token
newtype ExotelApiToken = ExotelApiToken
  { getExotelApiToken :: Text
  }
  deriving (Show)

deriveIdentifierInstances ''ExotelApiToken

-- | Exotel API token
newtype ExotelApiKey = ExotelApiKey
  { getExotelApiKey :: Text
  }
  deriving (Show)

deriveIdentifierInstances ''ExotelApiKey

-- | Exotel sid
newtype ExotelAccountSID = ExotelAccountSID
  { getExotelAccountSID :: Text
  }
  deriving (Show)

deriveIdentifierInstances ''ExotelAccountSID

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
  toForm exoReq =
    [ ("From", toQueryParam (from exoReq)),
      ("To", toQueryParam (to exoReq)),
      ("CallerId", toQueryParam (callerId exoReq))
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
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

-- | Call direction
data ExotelDirection
  = -- Incoming call
    INBOUND
  | -- Outbound calls from Exotel dashboard
    OUTBOUND_DIAL
  | -- All other Outbound calls (API, campaign etc.)
    OUTBOUND_API
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

-- | Exotel response body
data ExotelResponseBody = ExotelResponseBody
  { -- string; an alpha-numeric unique identifier of the call
    _exoSid :: ExotelCallSID,
    -- Time in format YYYY-MM-DD HH:mm:ss
    -- Date and time at which the user initiated the API
    _exoDateCreated :: Text,
    -- Time in format YYYY-MM-DD HH:mm:ss
    -- Date and time at which the status of the call
    -- was last updated in our system
    _exoDateUpdated :: Text,
    -- Exotel account SID
    _exoAccountSid :: ExotelAccountSID,
    -- Your customer's phone number
    _exoTo :: Text,
    -- The phone number that will be called first
    _exoFrom :: Text,
    -- This is your ExoPhone/Exotel Virtual Number
    _exoPhoneNumberSid :: Text,
    -- Overall call status
    _exoStatus :: ExotelCallStatus,
    -- Time in format YYYY-MM-DD HH:mm:ss
    -- Date and time when the call request was initiated to the operator
    _exoStartTime :: Text,
    -- Time in format YYYY-MM-DD HH:mm:ss
    -- Date and time when the call was completed
    _exoEndTime :: Text,
    -- Call duration in seconds
    _exoDuration :: Text,
    -- Double; If present, this will be the amount (in INR or USD) you have been charged for the call
    _exoPrice :: Text,
    -- Call direction
    _exoDirection :: ExotelDirection,
    -- human
    _exoAnsweredBy :: Text,
    -- Uri is the path of the CallSid
    _exoUri :: Text,
    -- Link to the call recording
    _exoRecordingUrl :: Text
  }
  deriving (Eq, Show)

$(makeLenses ''ExotelResponseBody)

$(deriveFromJSON (aesonPrefix snakeCase) ''ExotelResponseBody)

-- | Exotel response on success
newtype ExotelResponse = ExotelResponse
  { _exoCall :: ExotelResponseBody
  }
  deriving (Eq, Show)

$(makeLenses ''ExotelResponse)

$(deriveFromJSON (aesonPrefix snakeCase) ''ExotelResponse)
