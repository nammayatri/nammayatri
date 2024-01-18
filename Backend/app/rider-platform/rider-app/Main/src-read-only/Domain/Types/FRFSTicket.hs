{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.FRFSTicket where

import qualified Domain.Types.FRFSTicketBooking
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Id
import Tools.Beam.UtilsTH

data FRFSTicket = FRFSTicket
  { frfsTicketBookingId :: Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking,
    id :: Kernel.Types.Id.Id Domain.Types.FRFSTicket.FRFSTicket,
    qrData :: Kernel.Prelude.Text,
    status :: Domain.Types.FRFSTicket.FRFSTicketStatus,
    ticketNumber :: Kernel.Prelude.Text,
    validTill :: Kernel.Prelude.UTCTime,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data FRFSTicketStatus = ACTIVE | EXPIRED | USED
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(mkBeamInstancesForEnum ''FRFSTicketStatus)
