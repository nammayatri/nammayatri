{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.RideBooking.Confirm
  ( API.Types.Dashboard.RideBooking.Confirm.API,
    handler,
  )
where

import qualified API.Types.Dashboard.RideBooking.Confirm
import qualified "this" API.UI.Confirm
import qualified Domain.Action.Dashboard.RideBooking.Confirm
import qualified "this" Domain.Types.Extra.MerchantPaymentMethod
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.Person
import qualified "this" Domain.Types.Quote
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.External.Payment.Interface
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.Dashboard.RideBooking.Confirm.API)
handler merchantId city = postConfirmRideSearchQuotes merchantId city

postConfirmRideSearchQuotes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.Quote.Quote -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.External.Payment.Interface.PaymentMethodId -> Kernel.Prelude.Maybe Domain.Types.Extra.MerchantPaymentMethod.PaymentInstrument -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Environment.FlowHandler API.UI.Confirm.ConfirmRes)
postConfirmRideSearchQuotes a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Confirm.postConfirmRideSearchQuotes a9 a8 a7 a6 a5 a4 a3 a2 a1
