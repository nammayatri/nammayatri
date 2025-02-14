{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FRFSTicketBooking where

import qualified BecknV2.FRFS.Enums
import Data.Aeson
import qualified Domain.Types.FRFSQuote
import qualified Domain.Types.FRFSSearch
import qualified Domain.Types.Journey
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.PartnerOrganization
import qualified Domain.Types.Person
import qualified Domain.Types.Station
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Lib.JourneyLeg.Types
import qualified Tools.Beam.UtilsTH

data FRFSTicketBooking = FRFSTicketBooking
  { _type :: Domain.Types.FRFSQuote.FRFSQuoteType,
    bppBankAccountNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    bppBankCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    bppDelayedInterest :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    bppItemId :: Kernel.Prelude.Text,
    bppOrderId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    bppSubscriberId :: Kernel.Prelude.Text,
    bppSubscriberUrl :: Kernel.Prelude.Text,
    cancellationCharges :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    cashbackPayoutOrderId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    cashbackStatus :: Kernel.Prelude.Maybe Domain.Types.FRFSTicketBooking.CashbackStatus,
    customerCancelled :: Kernel.Prelude.Bool,
    discountedTickets :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    discountsJson :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    estimatedPrice :: Kernel.Types.Common.Price,
    eventDiscountAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    finalPrice :: Kernel.Prelude.Maybe Kernel.Types.Common.Price,
    fromStationId :: Kernel.Types.Id.Id Domain.Types.Station.Station,
    googleWalletJWTUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking,
    isBookingCancellable :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isDeleted :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isFareChanged :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isSkipped :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    journeyId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Journey.Journey),
    journeyLegOrder :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    journeyLegStatus :: Kernel.Prelude.Maybe Lib.JourneyLeg.Types.JourneyLegStatus,
    journeyOnInitDone :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    journeyRouteDetails :: [Lib.JourneyLeg.Types.MultiModalJourneyRouteDetails],
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    partnerOrgId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.PartnerOrganization.PartnerOrganization),
    partnerOrgTransactionId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.PartnerOrganization.PartnerOrgTransaction),
    payerVpa :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    paymentTxnId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    price :: Kernel.Types.Common.Price,
    providerDescription :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    providerId :: Kernel.Prelude.Text,
    providerName :: Kernel.Prelude.Text,
    quantity :: Kernel.Prelude.Int,
    quoteId :: Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote,
    refundAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    riderId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    routeStationsJson :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    searchId :: Kernel.Types.Id.Id Domain.Types.FRFSSearch.FRFSSearch,
    startTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    stationsJson :: Kernel.Prelude.Text,
    status :: Domain.Types.FRFSTicketBooking.FRFSTicketBookingStatus,
    toStationId :: Kernel.Types.Id.Id Domain.Types.Station.Station,
    validTill :: Kernel.Prelude.UTCTime,
    vehicleType :: BecknV2.FRFS.Enums.VehicleCategory,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show)

data CashbackStatus = PENDING | PROCESSING | SUCCESSFUL | CASHBACK_FAILED | MANUAL_VERIFICATION deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data FRFSTicketBookingStatus
  = NEW
  | APPROVED
  | PAYMENT_PENDING
  | CONFIRMING
  | FAILED
  | CONFIRMED
  | CANCELLED
  | COUNTER_CANCELLED
  | CANCEL_INITIATED
  | TECHNICAL_CANCEL_REJECTED
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''CashbackStatus)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''FRFSTicketBookingStatus)
