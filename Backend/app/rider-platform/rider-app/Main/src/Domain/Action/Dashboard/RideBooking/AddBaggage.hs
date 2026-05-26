module Domain.Action.Dashboard.RideBooking.AddBaggage
  ( postAddBaggageConfirm,
  )
where

import qualified API.Types.Dashboard.RideBooking.Endpoints.AddBaggage as DashboardTypes
import qualified "this" Domain.Action.UI.AddBaggage as DAddBaggage
import qualified "this" Domain.Types.Booking
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.Person
import qualified Environment
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import SharedLogic.Merchant (findMerchantByShortId)

postAddBaggageConfirm ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id Domain.Types.Booking.Booking ->
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  DashboardTypes.AddBaggageConfirmReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postAddBaggageConfirm merchantShortId _opCity bookingId customerId req = do
  m <- findMerchantByShortId merchantShortId
  let uiReq = DAddBaggage.AddBaggageConfirmReq {numberOfLuggages = req.numberOfLuggages}
  DAddBaggage.postAddBaggageConfirm (customerId, m.id) bookingId uiReq
