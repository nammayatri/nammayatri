module Beckn.External.MyValueFirst.Types where

import EulerHS.Prelude

data SubmitSms = SubmitSms
  { -- | Login of myfirstvalue.com account.
    _username :: Text,
    -- | Password of that account.
    _password :: Text,
    -- | Author name assigned to SMS.
    _from :: Text,
    -- | Phone number.
    _to :: Text,
    -- | SMS contents.
    _text :: Text
  }
  deriving (Show)
