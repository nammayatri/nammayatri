{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.EditBooking where

import Data.OpenApi (ToParamSchema, ToSchema)
import EulerHS.Prelude hiding (id)
import Servant
import Tools.Auth

data EditBookingRespondAPIReq = EditBookingRespondAPIReq {action :: API.Types.UI.EditBooking.EditBookingRespondAction} deriving (Generic, ToJSON, FromJSON, ToSchema)

data EditBookingRespondAction = ACCEPT | REJECT deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)
