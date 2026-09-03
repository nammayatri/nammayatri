{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.BookingDeposit
  ( API,
    handler,
  )
where

import qualified API.Types.UI.BookingDeposit
import qualified Control.Lens
import qualified Domain.Action.UI.BookingDeposit
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

type API = (TokenAuth :> "bookingDepositPaymentIntent" :> ReqBody ('[JSON]) API.Types.UI.BookingDeposit.BookingDepositPaymentIntentReq :> Post ('[JSON]) API.Types.UI.BookingDeposit.BookingDepositPaymentIntentResp)

handler :: Environment.FlowServer API
handler = postBookingDepositPaymentIntent

postBookingDepositPaymentIntent ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.BookingDeposit.BookingDepositPaymentIntentReq ->
    Environment.FlowHandler API.Types.UI.BookingDeposit.BookingDepositPaymentIntentResp
  )
postBookingDepositPaymentIntent a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.BookingDeposit.postBookingDepositPaymentIntent (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
