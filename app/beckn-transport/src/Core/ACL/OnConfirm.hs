module Core.ACL.OnConfirm where

import Beckn.Prelude
import qualified Beckn.Types.Core.Taxi.OnConfirm as OnConfirm
import Beckn.Types.Core.Taxi.OnConfirm.Order as OnConfimr
import Domain.Action.Beckn.Confirm

makeOnConfirmReq :: DOnConfirmReq -> OnConfirm.OnConfirmMessage
makeOnConfirmReq dOnConfirmReq =
  OnConfirm.OnConfirmMessage $
    OnConfirm.Order
      { id = dOnConfirmReq.rideBookingId.getId,
        items = [OnConfirm.OrderItem dOnConfirmReq.quoteId.getId],
        estimated_total_fare = OnConfirm.Price $ realToFrac dOnConfirmReq.estimatedTotalFare
      }
