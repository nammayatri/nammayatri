{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.ParkingBooking
  ( API,
    handler,
  )
where

import qualified API.Types.UI.ParkingBooking
import qualified Control.Lens
import qualified Data.Text
import qualified Domain.Action.UI.ParkingBooking
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( TokenAuth :> "multimodal" :> "parking" :> "book" :> Header "api-key" Data.Text.Text :> ReqBody ('[JSON]) API.Types.UI.ParkingBooking.ParkingBookingReq
      :> Post
           ('[JSON])
           API.Types.UI.ParkingBooking.ParkingBookingResponse
  )

handler :: Environment.FlowServer API
handler = postMultimodalParkingBook

postMultimodalParkingBook ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Maybe (Data.Text.Text) ->
    API.Types.UI.ParkingBooking.ParkingBookingReq ->
    Environment.FlowHandler API.Types.UI.ParkingBooking.ParkingBookingResponse
  )
postMultimodalParkingBook a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.ParkingBooking.postMultimodalParkingBook (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1
