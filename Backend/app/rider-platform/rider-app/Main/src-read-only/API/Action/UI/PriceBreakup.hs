{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.PriceBreakup
  ( API,
    handler,
  )
where

import qualified API.Types.UI.PriceBreakup
import qualified Control.Lens
import qualified Domain.Action.UI.PriceBreakup
import qualified Domain.Types.Booking
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

type API = (TokenAuth :> "priceBreakup" :> MandatoryQueryParam "bookingId" (Kernel.Types.Id.Id Domain.Types.Booking.Booking) :> Get '[JSON] API.Types.UI.PriceBreakup.QuoteBreakupRes)

handler :: Environment.FlowServer API
handler = getPriceBreakup

getPriceBreakup ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Booking.Booking ->
    Environment.FlowHandler API.Types.UI.PriceBreakup.QuoteBreakupRes
  )
getPriceBreakup a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.PriceBreakup.getPriceBreakup (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
