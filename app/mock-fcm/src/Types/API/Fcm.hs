module Types.API.Fcm where

import Beckn.External.FCM.Types
import Servant

type ReadFcmAPI =
  "read"
    :> Capture "token" FCMRecipientToken
    :> Get '[JSON] ReadFcmRes

type ReadFcmRes = [FCMMessage]
