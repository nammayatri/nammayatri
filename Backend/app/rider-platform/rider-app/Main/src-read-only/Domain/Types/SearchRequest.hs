{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.SearchRequest where

import qualified API.Types.UI.RiderLocation
import qualified BecknV2.OnDemand.Enums
import Data.Aeson
import qualified Domain.Types.Client
import qualified Domain.Types.Extra.MerchantPaymentMethod
import qualified Domain.Types.Location
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.MerchantPaymentMethod
import qualified Domain.Types.Person
import qualified Domain.Types.RecentLocation
import qualified Domain.Types.RefereeLink
import qualified Domain.Types.Trip
import qualified Kernel.External.Maps
import qualified Kernel.External.Payment.Interface.Types
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Types.Version
import Kernel.Utils.TH
import qualified Lib.Yudhishthira.Types
import qualified Tools.Beam.UtilsTH

data SearchRequest = SearchRequest
  { allJourneysLoaded :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    autoAssignEnabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    autoAssignEnabledV2 :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    availablePaymentMethods :: [Kernel.Types.Id.Id Domain.Types.MerchantPaymentMethod.MerchantPaymentMethod],
    backendAppVersion :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    backendConfigVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    busLocationData :: [API.Types.UI.RiderLocation.BusLocation],
    clientBundleVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    clientConfigVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    clientDevice :: Kernel.Prelude.Maybe Kernel.Types.Version.Device,
    clientId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Client.Client),
    clientReactNativeVersion :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    clientSdkVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    configInExperimentVersions :: [Lib.Yudhishthira.Types.ConfigVersionMap],
    createdAt :: Kernel.Prelude.UTCTime,
    customerExtraFee :: Kernel.Prelude.Maybe Kernel.Types.Common.Price,
    destinationStopCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    device :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    disabilityTag :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    distance :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance,
    distanceUnit :: Kernel.Types.Common.DistanceUnit,
    driverIdentifier :: Kernel.Prelude.Maybe Domain.Types.RefereeLink.DriverIdentifier,
    estimatedRideDuration :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    estimatedRideStaticDuration :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    fromLocation :: Domain.Types.Location.Location,
    fromSpecialLocationId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    hasMultimodalSearch :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    hasStops :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    id :: Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest,
    initiatedBy :: Kernel.Prelude.Maybe Domain.Types.Trip.TripParty,
    isAdvanceBookingEnabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isDashboardRequest :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isMeterRideSearch :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isMultimodalSearch :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isPetRide :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    language :: Kernel.Prelude.Maybe Kernel.External.Maps.Language,
    maxDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    multimodalSearchRequestId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    onSearchFailed :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    originStopCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    placeNameSource :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    recentLocationId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.RecentLocation.RecentLocation),
    returnTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    riderId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    riderPreferredOption :: Domain.Types.SearchRequest.RiderPreferredOption,
    roundTrip :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    routeCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    searchMode :: Kernel.Prelude.Maybe Domain.Types.SearchRequest.SearchMode,
    selectedPaymentInstrument :: Kernel.Prelude.Maybe Domain.Types.Extra.MerchantPaymentMethod.PaymentInstrument,
    selectedPaymentMethodId :: Kernel.Prelude.Maybe Kernel.External.Payment.Interface.Types.PaymentMethodId,
    startTime :: Kernel.Prelude.UTCTime,
    stops :: [Domain.Types.Location.Location],
    toLocation :: Kernel.Prelude.Maybe Domain.Types.Location.Location,
    toSpecialLocationId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    totalRidesCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    validTill :: Kernel.Prelude.UTCTime,
    vehicleCategory :: Kernel.Prelude.Maybe BecknV2.OnDemand.Enums.VehicleCategory
  }
  deriving (Generic, Show)

data RiderPreferredOption = Rental | OneWay | InterCity | Ambulance | Delivery | PublicTransport | FixedRoute deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data SearchMode = NORMAL | RESERVE deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data SearchRequestStatus = NEW | INPROGRESS | CONFIRMED | COMPLETED | CLOSED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''RiderPreferredOption)

$(mkHttpInstancesForEnum ''RiderPreferredOption)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''SearchMode)

$(mkHttpInstancesForEnum ''SearchMode)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''SearchRequestStatus)

$(mkHttpInstancesForEnum ''SearchRequestStatus)
