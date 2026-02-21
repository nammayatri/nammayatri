{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FRFSTicketBooking where

import qualified BecknV2.FRFS.Enums
import qualified Data.Aeson
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.FRFSQuote
import qualified Domain.Types.FRFSTicketBooking
import qualified Domain.Types.FRFSTicketBookingStatus
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data FRFSTicketBookingT f = FRFSTicketBookingT
  { _type :: B.C f Domain.Types.FRFSQuote.FRFSQuoteType,
    bookingAuthCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    bppBankAccountNumber :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    bppBankCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    bppDelayedInterest :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    bppItemId :: B.C f Kernel.Prelude.Text,
    bppOrderId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    bppSubscriberId :: B.C f Kernel.Prelude.Text,
    bppSubscriberUrl :: B.C f Kernel.Prelude.Text,
    busLocationData :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    cancellationCharges :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    cashbackPayoutOrderId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    cashbackStatus :: B.C f (Kernel.Prelude.Maybe Domain.Types.FRFSTicketBooking.CashbackStatus),
    customerCancelled :: B.C f Kernel.Prelude.Bool,
    discountedTickets :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    eventDiscountAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    failureReason :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    frfsTicketBookingPaymentIdForTicketGeneration :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    fromStationAddress :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    fromStationId :: B.C f Kernel.Prelude.Text,
    fromStationName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    fromStationLat :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    fromStationLon :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    fromStopIdx :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    googleWalletJWTUrl :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    holdId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    id :: B.C f Kernel.Prelude.Text,
    integratedBppConfigId :: B.C f Kernel.Prelude.Text,
    isBookingCancellable :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    isFareChanged :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    isMockPayment :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    isSingleMode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    journeyOnInitDone :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    multimodalSearchRequestId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    ondcOnInitReceived :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    ondcOnInitReceivedAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    osBuildVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    osType :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    partnerOrgId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    partnerOrgTransactionId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    payerVpa :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    paymentTxnId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    providerDescription :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    providerId :: B.C f Kernel.Prelude.Text,
    providerName :: B.C f Kernel.Prelude.Text,
    quoteId :: B.C f Kernel.Prelude.Text,
    recentLocationId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    refundAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    riderId :: B.C f Kernel.Prelude.Text,
    routeStationsJson :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    searchId :: B.C f Kernel.Prelude.Text,
    seatIds :: B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text]),
    seatLabels :: B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text]),
    startTime :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    stationsJson :: B.C f Kernel.Prelude.Text,
    status :: B.C f Domain.Types.FRFSTicketBookingStatus.FRFSTicketBookingStatus,
    toStationAddress :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    toStationId :: B.C f Kernel.Prelude.Text,
    toStationName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    toStationLat :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    toStationLon :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    toStopIdx :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    currency :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency),
    price :: B.C f Kernel.Types.Common.HighPrecMoney,
    tripId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    validTill :: B.C f Kernel.Prelude.UTCTime,
    vehicleNumber :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    vehicleType :: B.C f BecknV2.FRFS.Enums.VehicleCategory,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table FRFSTicketBookingT where
  data PrimaryKey FRFSTicketBookingT f = FRFSTicketBookingId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FRFSTicketBookingId . id

type FRFSTicketBooking = FRFSTicketBookingT Identity

$(enableKVPG ''FRFSTicketBookingT ['id] [['bppOrderId], ['quoteId], ['riderId], ['searchId]])

$(mkTableInstances ''FRFSTicketBookingT "frfs_ticket_booking")
