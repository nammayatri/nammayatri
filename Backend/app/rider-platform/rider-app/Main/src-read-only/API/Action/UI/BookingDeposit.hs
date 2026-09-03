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

type API =
  ( TokenAuth :> "bookingDeposit" :> Capture "bookingId" (Kernel.Types.Id.Id Domain.Types.Booking.Booking) :> "status"
      :> Get
           ('[JSON])
           API.Types.UI.BookingDeposit.BookingDepositStatusResp
      :<|> TokenAuth
      :> "bookingDeposit"
      :> Capture
           "bookingId"
           (Kernel.Types.Id.Id Domain.Types.Booking.Booking)
      :> "paymentIntent"
      :> Post
           ('[JSON])
           API.Types.UI.BookingDeposit.BookingDepositPaymentResp
      :<|> TokenAuth
      :> "bookingDeposit"
      :> Capture
           "bookingId"
           (Kernel.Types.Id.Id Domain.Types.Booking.Booking)
      :> "refund"
      :> Post
           ('[JSON])
           API.Types.UI.BookingDeposit.BookingDepositStatusResp
  )

handler :: Environment.FlowServer API
handler = getBookingDepositStatus :<|> postBookingDepositPaymentIntent :<|> postBookingDepositRefund

getBookingDepositStatus ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Booking.Booking ->
    Environment.FlowHandler API.Types.UI.BookingDeposit.BookingDepositStatusResp
  )
getBookingDepositStatus a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.BookingDeposit.getBookingDepositStatus (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postBookingDepositPaymentIntent ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Booking.Booking ->
    Environment.FlowHandler API.Types.UI.BookingDeposit.BookingDepositPaymentResp
  )
postBookingDepositPaymentIntent a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.BookingDeposit.postBookingDepositPaymentIntent (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postBookingDepositRefund ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Booking.Booking ->
    Environment.FlowHandler API.Types.UI.BookingDeposit.BookingDepositStatusResp
  )
postBookingDepositRefund a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.BookingDeposit.postBookingDepositRefund (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
