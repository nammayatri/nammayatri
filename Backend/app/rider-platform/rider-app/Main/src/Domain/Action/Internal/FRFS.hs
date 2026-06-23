{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Internal.FRFS
  ( frfsStatusUpdate,
    FRFSStatusUpdateReq (..),
  )
where

import Data.List (nubBy, sortBy)
import Domain.Action.UI.Payment
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Finance.Core.Types (Actor)
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import qualified Storage.Queries.FRFSTicketBookingPayment as QFRFSTicketBookingPayment
import Tools.Error

newtype FRFSStatusUpdateReq = FRFSStatusUpdateReq
  { bookingIds :: [Id DFRFSTicketBooking.FRFSTicketBooking]
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

---------------------------------------------------------------------
frfsStatusUpdate ::
  FRFSStatusUpdateReq ->
  Actor ->
  Flow APISuccess
frfsStatusUpdate req actor = do
  let reqBookingIds = req.bookingIds
  mapM_
    ( \bookingId -> do
        withTryCatch "frfsStatusUpdate" $ do
          booking <- QFRFSTicketBooking.findById bookingId >>= fromMaybeM (InvalidRequest "Invalid booking payment id")
          let merchantId = booking.merchantId
              personId = booking.riderId
          bookingPayments <- QFRFSTicketBookingPayment.findAllTBPByBookingId bookingId
          let uniqueBookingPayments = nubBy ((==) `on` (.paymentOrderId)) $ sortBy (compare `on` (.paymentOrderId)) bookingPayments
          mapM_ (\paymentBooking -> getStatus (personId, merchantId) paymentBooking.paymentOrderId actor) uniqueBookingPayments
    )
    reqBookingIds

  pure Success
