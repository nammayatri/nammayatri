{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SMS.MyValueFirst.Types where

import Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import EulerHS.Prelude hiding (encodeUtf8, fromStrict, toStrict)
import Kernel.Types.Servant
import Servant

data SubmitSms = SubmitSms
  { -- | Login of myfirstvalue.com account.
    username :: Text,
    -- | Password of that account.
    password :: Text,
    -- | Author name assigned to SMS.
    from :: Text,
    -- | Phone number.
    to :: Text,
    -- | SMS contents.
    text :: Text
  }
  deriving (Show)

data SubmitSmsRes
  = Sent
  | BadNumber
  | InvalidReceiver
  | EmptyNumber
  | MissingSender
  | EmptyText
  | UnknownError
  | AuthorizationFailure
  deriving (Generic, FromJSON, ToJSON, Show, Eq)

instance MimeUnrender PlainText_ISO_8859_1 SubmitSmsRes where
  mimeUnrender _ = Right . parseSubmitSmsRes . T.decodeLatin1 . toStrict

instance MimeRender PlainText_ISO_8859_1 SubmitSmsRes where
  mimeRender _ = fromStrict . T.encodeUtf8 . submitSmsResToText

parseSubmitSmsRes :: Text -> SubmitSmsRes
parseSubmitSmsRes txt
  | "Sent." `T.isPrefixOf` txt = Sent
  | "Number" `T.isPrefixOf` txt = BadNumber
  | "Invalid Receiver" `T.isPrefixOf` txt = InvalidReceiver
  | "Empty receiver number" `T.isPrefixOf` txt = EmptyNumber
  | "Sender" `T.isPrefixOf` txt = MissingSender
  | "Empty text not allowed" `T.isPrefixOf` txt = EmptyText
  | "unknown" `T.isPrefixOf` txt = UnknownError
  | "Authorization" `T.isPrefixOf` txt = AuthorizationFailure
  | otherwise = UnknownError

submitSmsResToText :: SubmitSmsRes -> Text
submitSmsResToText = \case
  Sent -> "Sent."
  BadNumber -> "Number(s) has/have been denied by white-and/or black-lists."
  InvalidReceiver -> "Invalid Receiver"
  EmptyNumber -> "Empty receiver number not allowed, rejected"
  MissingSender -> "Sender missing and no global set, rejected"
  EmptyText -> "Empty text not allowed, rejected."
  UnknownError -> "unknown request"
  AuthorizationFailure -> "Authorization failed for sendsms"
