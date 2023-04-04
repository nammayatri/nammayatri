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

module SMS.ExotelSms.Types where

import Control.Lens.TH
import Data.Aeson.Casing
import Data.Aeson.TH
import Data.OpenApi (ToSchema)
import Data.Text as T
import Data.Text.Conversions
import EulerHS.Prelude
import Kernel.External.Call.Exotel.Types (ExotelAccountSID)
import Kernel.Storage.Esqueleto (derivePersistField)
import Kernel.Utils.JSON
import Kernel.Utils.TH
import Web.FormUrlEncoded (ToForm, toForm)
import Web.Internal.HttpApiData

type OtpTemplate = Text

-- | Exotel Url token
newtype ExotelURL = ExotelURL
  { getExotelAccountURL :: String
  }
  deriving newtype (Show)

deriveIdentifierInstances ''ExotelURL

data SubmitSmsReq = SubmitSmsReq
  { from :: Text,
    -- String; Your customer's phone number.
    -- If landline number, prefix it with STD code; Ex: 080XXXX2400
    to :: Text,
    -- Mobile number to which SMS has to be sent. Preferably in E.164 format. If not set, our system will try to match it with a
    -- country and route the SMS
    body :: Text
    -- 	String; Content of your SMS; Max Length of the body cannot exceed
    -- 2000 characters
  }
  deriving (Generic, Eq, Show)

instance ToForm SubmitSmsReq where
  toForm SubmitSmsReq {..} =
    [ ("From", toQueryParam from),
      ("To", toQueryParam to),
      ("Body", toQueryParam body)
    ]

-- | SMS direction
data ExotelSmsDirection
  = INBOUND
  | -- Incoming call

    OUTBOUND_CALL
  | --  messages initiated during a call
    OUTBOUND_API
  | -- messages initiated during a call

    OUTBOUND_REPLY
  -- messages initiated in response to an incoming message
  deriving (Show, Eq, Read, Generic, ToSchema)

$(deriveJSON constructorsWithHyphensToLowerOptions ''ExotelSmsDirection)

-- | Overall sms status
data ExotelSmsStatus
  = QUEUED
  | -- SMS you have sent has been queued internally for delivery
    SENDING
  | -- in process of sending the SMS to upstream provider/gateway
    FAILED_DND
  | -- The delivery of the SMS failed because the number belongs to the TRAI NCPR list. This is applicable only for promotional SMS
    SUBMITTED
  | -- This means that the SMS you have sent has been submitted from our system to SMS Gateway
    SENT
  | --  The SMS was successfully delivered to the handset
    FAILED
  | -- The delivery of SMS failed. Please see DetailedStatusCode and DetailedStatus for further details.

    INVALID_STATUS
  deriving (Show, Eq, Read, Generic, ToSchema)

instance FromText ExotelSmsStatus where
  fromText a =
    case a of
      "queued" -> QUEUED
      "sending" -> SENDING
      "failed-dnd" -> FAILED_DND
      "submitted" -> SUBMITTED
      "failed" -> FAILED
      "sent" -> SENT
      _ -> INVALID_STATUS

$(deriveJSON constructorsWithHyphensToLowerOptions ''ExotelSmsStatus)

derivePersistField "ExotelSmsStatus"

-- an alpha-numeric unique identifier of the SMS
newtype ExotelSmsSID = ExotelSmsSID
  { getExotelSmsSID :: Text
  }
  deriving (Show)

deriveIdentifierInstances ''ExotelSmsSID

-- | Exotel response body
data SubmitSmsRespBody = SubmitSmsRespBody
  { exoSid :: ExotelSmsSID,
    -- string; an alpha-numeric unique identifier of the call
    exoAccountSid :: ExotelAccountSID,
    -- Exotel account SID
    exoFrom :: Text,
    -- The phone number that will be called first
    exoTo :: Text,
    -- Your customer's phone number
    exoDateCreated :: Text,
    -- Time in format YYYY-MM-DD HH:mm:ss; The time when the SMS was received at our server
    exoDateUpdated :: Text,
    -- 	Time in format YYYY-MM-DD HH:mm:ss; The time when any property of this SMS was last updated by our server
    exoDateSent :: Maybe Text,
    -- Time in format YYYY-MM-DD HH:mm:ss; The time when the SMS was delivered to the recepient
    exoBody :: Text,
    -- String; The body of the SMS message sent
    exoStatus :: ExotelSmsStatus,
    exoDetailedStatus :: Maybe Text,
    exoDetailedStatusCode :: Maybe Int,
    exoPrice :: Maybe Double,
    exoDirection :: ExotelSmsDirection,
    -- 	Exotel’s Detailed Status code corresponding to the DetailedStatus. Use this field to build decision making in your code
    exoUri :: Text,
    exoSmsUnits :: Maybe Int
  }
  deriving (Eq, Show, Generic)

$(makeLenses ''SubmitSmsRespBody)

$(deriveFromJSON (aesonPrefix pascalCase) ''SubmitSmsRespBody)
$(deriveToJSON (aesonPrefix pascalCase) ''SubmitSmsRespBody)

--  Exotel Sms response on success
newtype SubmitSmsResp = SubmitSmsResp
  { exoSMSMessage :: SubmitSmsRespBody
  }
  deriving (Eq, Show, Generic)

$(makeLenses ''SubmitSmsResp)

$(deriveFromJSON (aesonPrefix pascalCase) ''SubmitSmsResp)
$(deriveToJSON (aesonPrefix pascalCase) ''SubmitSmsResp)
