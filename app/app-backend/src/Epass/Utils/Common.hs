module Epass.Utils.Common where

import Beckn.Utils.Common
import qualified Data.ByteString.Lazy as BSL
import Data.Time
import Data.Time.Calendar (Day (..))
import Data.Time.Clock
import Data.Time.LocalTime
import Epass.Types.Common
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant

sendAck :: L.Flow Ack
sendAck =
  pure $
    Ack
      { _action = "ACK",
        _message = "OK"
      }
