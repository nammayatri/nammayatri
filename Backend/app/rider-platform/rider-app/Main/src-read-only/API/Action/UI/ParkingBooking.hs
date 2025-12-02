{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.ParkingBooking
  ( API,
    handler,
  )
where

import qualified API.Types.UI.ParkingBooking
import qualified Data.Text
import qualified Domain.Action.UI.ParkingBooking
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( "multimodal" :> "parking" :> "book" :> Header "api-key" Data.Text.Text :> ReqBody '[JSON] API.Types.UI.ParkingBooking.ParkingBookingReq
      :> Post
           '[JSON]
           API.Types.UI.ParkingBooking.ParkingBookingResponse
  )

handler :: Environment.FlowServer API
handler = postMultimodalParkingBook

postMultimodalParkingBook :: (Kernel.Prelude.Maybe Data.Text.Text -> API.Types.UI.ParkingBooking.ParkingBookingReq -> Environment.FlowHandler API.Types.UI.ParkingBooking.ParkingBookingResponse)
postMultimodalParkingBook a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.ParkingBooking.postMultimodalParkingBook a2 a1
