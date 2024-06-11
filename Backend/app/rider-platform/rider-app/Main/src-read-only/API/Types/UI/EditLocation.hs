{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.EditLocation where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.BookingUpdateRequest
import EulerHS.Prelude hiding (id)
import Servant
import Tools.Auth

data EditLocationResultAPIResp = EditLocationResultAPIResp {bookingUpdateRequestDetails :: Domain.Types.BookingUpdateRequest.BookingUpdateRequest} deriving (Generic, ToJSON, FromJSON, ToSchema)

{-
	DSL Source Link: file://./../../../../spec/API/EditLocation.yaml
-}
