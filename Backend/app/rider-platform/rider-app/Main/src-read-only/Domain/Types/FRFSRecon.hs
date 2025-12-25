{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FRFSRecon where

import Data.Aeson
import qualified Domain.Types.FRFSTicketBooking
import qualified Domain.Types.FRFSTicketStatus
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data FRFSRecon = FRFSRecon
  { beneficiaryBankAccount :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    beneficiaryIFSC :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    buyerFinderFee :: Kernel.Types.Common.Price,
    collectorIFSC :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    collectorSubscriberId :: Kernel.Prelude.Text,
    date :: Kernel.Prelude.Text,
    destinationStationCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    differenceAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.Price,
    entityType :: Kernel.Prelude.Maybe Domain.Types.FRFSRecon.EntityType,
    fare :: Kernel.Types.Common.Price,
    frfsTicketBookingId :: Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking,
    id :: Kernel.Types.Id.Id Domain.Types.FRFSRecon.FRFSRecon,
    message :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    mobileNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    networkOrderId :: Kernel.Prelude.Text,
    paymentGateway :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    providerId :: Kernel.Prelude.Text,
    providerName :: Kernel.Prelude.Text,
    receiverSubscriberId :: Kernel.Prelude.Text,
    reconStatus :: Kernel.Prelude.Maybe Domain.Types.FRFSRecon.ReconStatus,
    settlementAmount :: Kernel.Types.Common.Price,
    settlementDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    settlementReferenceNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    sourceStationCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    ticketNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    ticketQty :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    ticketStatus :: Kernel.Prelude.Maybe Domain.Types.FRFSTicketStatus.FRFSTicketStatus,
    time :: Kernel.Prelude.Text,
    totalOrderValue :: Kernel.Types.Common.Price,
    transactionRefNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    transactionUUID :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    txnId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show)

data EntityType = BUS_PASS | FRFS_TICKET_BOOKING deriving (Show, (Eq), (Ord), (Read), (Generic), (ToJSON), (FromJSON), (ToSchema), (Kernel.Prelude.ToParamSchema))

data ReconStatus = PENDING | SETTLED | PARTIALLY_SETTLED | REFUNDED deriving (Show, (Eq), (Ord), (Read), (Generic), (ToJSON), (FromJSON), (ToSchema), (Kernel.Prelude.ToParamSchema))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum (''EntityType))

$(Kernel.Utils.TH.mkFromHttpInstanceForEnum (''EntityType))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum (''ReconStatus))

$(Kernel.Utils.TH.mkFromHttpInstanceForEnum (''ReconStatus))
