{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FRFSRecon where

import qualified Domain.Types.FRFSTicketBooking
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data FRFSRecon = FRFSRecon
  { beneficiaryBankAccount :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    beneficiaryIFSC :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    buyerFinderFee :: Kernel.Types.Common.HighPrecMoney,
    collectorIFSC :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    collectorSubscriberId :: Kernel.Prelude.Text,
    date :: Kernel.Prelude.Text,
    destinationStationCode :: Kernel.Prelude.Text,
    differenceAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    fare :: Kernel.Types.Common.HighPrecMoney,
    frfsTicketBookingId :: Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking,
    id :: Kernel.Types.Id.Id Domain.Types.FRFSRecon.FRFSRecon,
    message :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    mobileNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    networkOrderId :: Kernel.Prelude.Text,
    receiverSubscriberId :: Kernel.Prelude.Text,
    settlementAmount :: Kernel.Types.Common.HighPrecMoney,
    settlementDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    settlementReferenceNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    sourceStationCode :: Kernel.Prelude.Text,
    ticketNumber :: Kernel.Prelude.Text,
    ticketQty :: Kernel.Prelude.Int,
    time :: Kernel.Prelude.Text,
    totalOrderValue :: Kernel.Types.Common.HighPrecMoney,
    transactionRefNumber :: Kernel.Prelude.Text,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
