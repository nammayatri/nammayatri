module Beckn.External.MyValuesFirst.Types where

import           EulerHS.Prelude

data SubmitSms =
  SubmitSms
    { _username :: Text  -- ^ Login of myfirstvalue.com account.
    , _password :: Text  -- ^ Password of that account.
    , _from     :: Text  -- ^ Author name assigned to SMS.
    , _to       :: Text  -- ^ Phone number.
    , _text     :: Text  -- ^ SMS contents.
    }
  deriving (Show)

