module Domain.Action.Dashboard.RideBooking.ChangeServiceTier
  ( getChangeServiceTierQuotes,
    postChangeServiceTierConfirm,
  )
where

import qualified API.Types.Dashboard.RideBooking.Endpoints.ChangeServiceTier as DashboardTypes
import qualified "this" Domain.Action.UI.ChangeServiceTier as DChangeServiceTier
import qualified "this" Domain.Action.UI.Quote as DQuote
import qualified "this" Domain.Types.Booking
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.Person
import qualified Environment
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import SharedLogic.Merchant (findMerchantByShortId)

getChangeServiceTierQuotes ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id Domain.Types.Booking.Booking ->
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  Environment.Flow DQuote.GetQuotesRes
getChangeServiceTierQuotes merchantShortId _opCity bookingId customerId = do
  m <- findMerchantByShortId merchantShortId
  DChangeServiceTier.getChangeServiceTierQuotes (customerId, m.id) bookingId

postChangeServiceTierConfirm ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id Domain.Types.Booking.Booking ->
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  DashboardTypes.ChangeServiceTierConfirmReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postChangeServiceTierConfirm merchantShortId _opCity bookingId customerId req = do
  m <- findMerchantByShortId merchantShortId
  let uiReq = DChangeServiceTier.ChangeServiceTierConfirmReq {quoteId = req.quoteId}
  DChangeServiceTier.postChangeServiceTierConfirm (customerId, m.id) bookingId uiReq
