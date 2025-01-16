module Domain.Action.Dashboard.RideBooking.Confirm (postConfirmRideSearchQuotes) where

import qualified "this" API.UI.Confirm
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.Person
import qualified "this" Domain.Types.Quote
import qualified Environment
import qualified Kernel.External.Payment.Interface
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import SharedLogic.Merchant (findMerchantByShortId)

postConfirmRideSearchQuotes ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  Kernel.Types.Id.Id Domain.Types.Quote.Quote ->
  Kernel.Prelude.Maybe Kernel.External.Payment.Interface.PaymentMethodId ->
  Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
  Environment.Flow API.UI.Confirm.ConfirmRes
postConfirmRideSearchQuotes merchantShortId _opCity personId quoteId mbPaymentMethodId isAdvanceBookingEnabled = do
  m <- findMerchantByShortId merchantShortId
  API.UI.Confirm.confirm' (personId, m.id) quoteId mbPaymentMethodId isAdvanceBookingEnabled
