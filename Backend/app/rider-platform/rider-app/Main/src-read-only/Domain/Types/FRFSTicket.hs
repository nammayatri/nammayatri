{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FRFSTicket where

import Data.Aeson
import qualified Domain.Types.FRFSTicketBooking
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.PartnerOrganization
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data FRFSTicket = FRFSTicket
  { description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    frfsTicketBookingId :: Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking,
    id :: Kernel.Types.Id.Id Domain.Types.FRFSTicket.FRFSTicket,
    isTicketFree :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    partnerOrgId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.PartnerOrganization.PartnerOrganization),
    partnerOrgTransactionId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.PartnerOrganization.PartnerOrgTransaction),
    qrData :: Kernel.Prelude.Text,
    qrRefreshAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    riderId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    status :: Domain.Types.FRFSTicket.FRFSTicketStatus,
    ticketNumber :: Kernel.Prelude.Text,
    validTill :: Kernel.Prelude.UTCTime,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data FRFSTicketStatus = ACTIVE | EXPIRED | USED | CANCELLED | COUNTER_CANCELLED | CANCEL_INITIATED | TECHNICAL_CANCEL_REJECTED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''FRFSTicketStatus)
