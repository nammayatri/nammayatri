{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Types.UI.EditLocation where
import EulerHS.Prelude hiding (id)
import Servant
import Tools.Auth
import Data.OpenApi (ToSchema)
import qualified Domain.Types.BookingUpdateRequest



data EditLocationResultAPIResp
    = EditLocationResultAPIResp {bookingUpdateRequestDetails :: Domain.Types.BookingUpdateRequest.BookingUpdateRequest}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)



