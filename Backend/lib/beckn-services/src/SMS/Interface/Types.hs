{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module SMS.Interface.Types
  ( module SMS.Interface.Types,
  )
where

import Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Deriving.Aeson
import Kernel.Prelude
import Kernel.Types.Servant
import qualified SMS.ExotelSms.Config as ExotelSms
import qualified SMS.MyValueFirst.Config as MyValueFirst
import qualified SMS.Types as T
import Servant

data SmsHandler m = SmsHandler
  { getProvidersPriorityList :: m [T.SmsService],
    getProviderConfig :: T.SmsService -> m SmsServiceConfig
  }

data SmsServiceConfig = MyValueFirstConfig MyValueFirst.MyValueFirstCfg | ExotelSmsConfig ExotelSms.ExotelSmsCfg
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[SumTaggedObject "tag" "content"] SmsServiceConfig

data SendSMSReq = SendSMSReq
  { smsBody :: Text,
    phoneNumber :: Text,
    sender :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

data SendSMSRes = Success | Fail | Pending | UnknownError
  deriving (Generic, FromJSON, ToJSON, Show, Eq)

instance MimeUnrender PlainText_ISO_8859_1 SendSMSRes where
  mimeUnrender _ = Right . parseSendSMSRes . T.decodeLatin1 . toStrict

instance MimeRender PlainText_ISO_8859_1 SendSMSRes where
  mimeRender _ = fromStrict . T.encodeUtf8 . sendOtpResToText

parseSendSMSRes :: Text -> SendSMSRes
parseSendSMSRes txt
  | "Success" `T.isPrefixOf` txt = Success
  | "Fail" `T.isPrefixOf` txt = Fail
  | "Pending" `T.isPrefixOf` txt = Pending
  | otherwise = UnknownError

sendOtpResToText :: SendSMSRes -> Text
sendOtpResToText = \case
  Success -> "Success sms"
  Fail -> "Fail"
  Pending -> "Pending"
  UnknownError -> "unknown request"
