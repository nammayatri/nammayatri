module Types.API.Fcm where

import Beckn.External.FCM.Types
import Data.Aeson
import Servant

type ReadFcmAPI =
  "read"
    :> Capture "token" FCMRecipientToken
    :> Get '[JSON] ReadFcmRes

type ReadFcmRes = [FCMMessage Value]
