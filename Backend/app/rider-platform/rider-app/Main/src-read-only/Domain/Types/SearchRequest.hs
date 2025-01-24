{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.SearchRequest where

import Data.Aeson
import qualified Domain.Types.Client
import qualified Domain.Types.Location
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.MerchantPaymentMethod
import qualified Domain.Types.Person
import qualified Domain.Types.RefereeLink
import qualified Domain.Types.Trip
import qualified Kernel.External.Maps
import qualified Kernel.External.Payment.Interface.Types
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Types.Version
import Kernel.Utils.TH
import qualified Lib.JourneyLeg.Types
import qualified Lib.Yudhishthira.Types
import qualified Tools.Beam.UtilsTH

data SearchRequest = SearchRequest
  { autoAssignEnabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    autoAssignEnabledV2 :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    availablePaymentMethods :: [Kernel.Types.Id.Id Domain.Types.MerchantPaymentMethod.MerchantPaymentMethod],
    backendAppVersion :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    backendConfigVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    clientBundleVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    clientConfigVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    clientDevice :: Kernel.Prelude.Maybe Kernel.Types.Version.Device,
    clientId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Client.Client),
    clientReactNativeVersion :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    clientSdkVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    configInExperimentVersions :: [Lib.Yudhishthira.Types.ConfigVersionMap],
    createdAt :: Kernel.Prelude.UTCTime,
    customerExtraFee :: Kernel.Prelude.Maybe Kernel.Types.Common.Price,
    device :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    disabilityTag :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    distance :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance,
    distanceUnit :: Kernel.Types.Common.DistanceUnit,
    driverIdentifier :: Kernel.Prelude.Maybe Domain.Types.RefereeLink.DriverIdentifier,
    estimatedRideDuration :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    estimatedRideStaticDuration :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    fromLocation :: Domain.Types.Location.Location,
    hasMultimodalSearch :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    hasStops :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    id :: Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest,
    initiatedBy :: Kernel.Prelude.Maybe Domain.Types.Trip.TripParty,
    isAdvanceBookingEnabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isDashboardRequest :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    journeyLegInfo :: Kernel.Prelude.Maybe Lib.JourneyLeg.Types.JourneySearchData,
    language :: Kernel.Prelude.Maybe Kernel.External.Maps.Language,
    maxDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    placeNameSource :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    returnTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    riderId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    riderPreferredOption :: Domain.Types.SearchRequest.RiderPreferredOption,
    roundTrip :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    selectedPaymentMethodId :: Kernel.Prelude.Maybe Kernel.External.Payment.Interface.Types.PaymentMethodId,
    startTime :: Kernel.Prelude.UTCTime,
    stops :: [Domain.Types.Location.Location],
    toLocation :: Kernel.Prelude.Maybe Domain.Types.Location.Location,
    totalRidesCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    validTill :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show)

data RiderPreferredOption = Rental | OneWay | InterCity | Ambulance | Delivery deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data SearchRequestStatus = NEW | INPROGRESS | CONFIRMED | COMPLETED | CLOSED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''RiderPreferredOption)

$(mkHttpInstancesForEnum ''RiderPreferredOption)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''SearchRequestStatus)

$(mkHttpInstancesForEnum ''SearchRequestStatus)
