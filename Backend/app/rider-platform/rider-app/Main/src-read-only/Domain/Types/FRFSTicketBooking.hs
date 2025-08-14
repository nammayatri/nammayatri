{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FRFSTicketBooking where

import qualified BecknV2.FRFS.Enums
import Data.Aeson
import qualified Domain.Types.FRFSQuote
import qualified Domain.Types.FRFSSearch
import qualified Domain.Types.FRFSTicketBookingStatus
import qualified Domain.Types.IntegratedBPPConfig
import qualified Domain.Types.Journey
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.PartnerOrganization
import qualified Domain.Types.Person
import qualified Domain.Types.RecentLocation
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data FRFSTicketBooking = FRFSTicketBooking
  { _type :: Domain.Types.FRFSQuote.FRFSQuoteType,
    bookingAuthCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
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
    childTicketQuantity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    customerCancelled :: Kernel.Prelude.Bool,
    discountedTickets :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    discountsJson :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    estimatedPrice :: Kernel.Types.Common.Price,
    eventDiscountAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    finalPrice :: Kernel.Prelude.Maybe Kernel.Types.Common.Price,
    fromStationCode :: Kernel.Prelude.Text,
    googleWalletJWTUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking,
    integratedBppConfigId :: Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig,
    isBookingCancellable :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isDeleted :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isFareChanged :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    journeyId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Journey.Journey),
    journeyLegOrder :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    journeyOnInitDone :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    osBuildVersion :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    osType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
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
    recentLocationId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.RecentLocation.RecentLocation),
    refundAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    riderId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    routeStationsJson :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    searchId :: Kernel.Types.Id.Id Domain.Types.FRFSSearch.FRFSSearch,
    startTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    stationsJson :: Kernel.Prelude.Text,
    status :: Domain.Types.FRFSTicketBookingStatus.FRFSTicketBookingStatus,
    toStationCode :: Kernel.Prelude.Text,
    validTill :: Kernel.Prelude.UTCTime,
    vehicleType :: BecknV2.FRFS.Enums.VehicleCategory,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show)

data CashbackStatus = PENDING | PROCESSING | SUCCESSFUL | CASHBACK_FAILED | MANUAL_VERIFICATION deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''CashbackStatus)
