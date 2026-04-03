{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.UI.PriceBreakup 
( API,
handler )
where
import Storage.Beam.SystemConfigs ()
import EulerHS.Prelude
import Servant
import Tools.Auth
import Kernel.Utils.Common
import qualified Domain.Action.UI.PriceBreakup
import qualified Domain.Types.Person
import qualified Kernel.Prelude
import qualified Control.Lens
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Id
import qualified Domain.Types.Booking
import qualified API.Types.UI.PriceBreakup



type API = (TokenAuth :> "priceBreakup" :> MandatoryQueryParam "bookingId" (Kernel.Types.Id.Id Domain.Types.Booking.Booking) :> Get ('[JSON]) API.Types.UI.PriceBreakup.QuoteBreakupRes)
handler :: Environment.FlowServer API
handler = getPriceBreakup
getPriceBreakup :: ((Kernel.Types.Id.Id Domain.Types.Person.Person,
                     Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id Domain.Types.Booking.Booking -> Environment.FlowHandler API.Types.UI.PriceBreakup.QuoteBreakupRes)
getPriceBreakup a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.PriceBreakup.getPriceBreakup (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1



