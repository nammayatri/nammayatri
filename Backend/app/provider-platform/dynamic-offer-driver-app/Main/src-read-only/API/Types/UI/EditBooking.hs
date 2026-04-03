{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Types.UI.EditBooking where
import EulerHS.Prelude hiding (id)
import Servant
import Tools.Auth
import Data.OpenApi (ToSchema)




data EditBookingRespondAPIReq
    = EditBookingRespondAPIReq {action :: EditBookingRespondAction}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)
data EditBookingRespondAction
    = ACCEPT | REJECT
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)



