module SharedLogic.FRFSPassInstantBook where

import qualified Domain.Types.FRFSSearch as DFRFSSearch
import qualified Domain.Types.FRFSTicketBookingStatus as DFRFSTicketBookingStatus
import qualified Domain.Types.PurchasedPassPayment as DPPP
import Kernel.Prelude
import Kernel.Types.Id

data InProgress = InProgress
  { searchId :: Maybe (Id DFRFSSearch.FRFSSearch),
    startedAt :: UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data RecentBooking = RecentBooking
  { status :: DFRFSTicketBookingStatus.FRFSTicketBookingStatus,
    searchId :: Id DFRFSSearch.FRFSSearch,
    createdAt :: UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)

inProgressKey :: Id DPPP.PurchasedPassPayment -> Text
inProgressKey purchasedPassPaymentId = "FRFS:PassInstantBook:InProgress:" <> purchasedPassPaymentId.getId

bookingsKey :: Id DPPP.PurchasedPassPayment -> Text
bookingsKey purchasedPassPaymentId = "FRFS:PassInstantBook:Bookings:" <> purchasedPassPaymentId.getId

lockKey :: Id DPPP.PurchasedPassPayment -> Text
lockKey purchasedPassPaymentId = "FRFS:PassInstantBook:Lock:" <> purchasedPassPaymentId.getId
