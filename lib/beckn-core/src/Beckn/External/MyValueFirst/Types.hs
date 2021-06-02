module Beckn.External.MyValueFirst.Types where

import EulerHS.Prelude

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
