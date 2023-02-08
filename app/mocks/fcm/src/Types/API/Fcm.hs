module Types.API.Fcm where

import Data.Aeson
import Kernel.External.FCM.Types
import Servant

type ReadFcmAPI =
  "read"
    :> Capture "token" FCMRecipientToken
    :> Get '[JSON] ReadFcmRes

type ReadFcmRes = [FCMMessage Value]
