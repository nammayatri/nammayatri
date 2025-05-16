{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.SearchRequest where

import qualified BecknV2.OnDemand.Enums
import qualified Data.Aeson
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.RefereeLink
import qualified Domain.Types.SearchRequest
import qualified Domain.Types.Trip
import Kernel.External.Encryption
import qualified Kernel.External.Maps
import qualified Kernel.External.Payment.Interface.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Version
import qualified Kernel.Utils.Common
import Tools.Beam.UtilsTH

data SearchRequestT f = SearchRequestT
  { allJourneysLoaded :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    autoAssignEnabled :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    autoAssignEnabledV2 :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    availablePaymentMethods :: B.C f [Kernel.Prelude.Text],
    backendAppVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    backendConfigVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientBundleVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientConfigVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientManufacturer :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientModelName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientOsType :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Version.DeviceType),
    clientOsVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientReactNativeVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientSdkVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    configInExperimentVersions :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    currency :: B.C f (Kernel.Prelude.Maybe Kernel.Utils.Common.Currency),
    customerExtraFee :: B.C f (Kernel.Prelude.Maybe Kernel.Utils.Common.Money),
    customerExtraFeeAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    destinationStopCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    device :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    disabilityTag :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    distance :: B.C f (Kernel.Prelude.Maybe Kernel.Utils.Common.Centesimal),
    distanceValue :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecDistance),
    distanceUnit :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit),
    driverIdentifierType :: B.C f (Kernel.Prelude.Maybe Domain.Types.RefereeLink.DriverIdentifierType),
    driverIdentifierValue :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    estimatedRideDuration :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds),
    estimatedRideStaticDuration :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds),
    fromLocationId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    hasMultimodalSearch :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    hasStops :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    id :: B.C f Kernel.Prelude.Text,
    initiatedBy :: B.C f (Kernel.Prelude.Maybe Domain.Types.Trip.TripParty),
    isAdvanceBookingEnabled :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    isDashboardRequest :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    isMeterRideSearch :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    agency :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    convenienceCost :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    isDeleted :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    journeyId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    journeyLegOrder :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    onSearchFailed :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    pricingId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    skipBooking :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    language :: B.C f (Kernel.Prelude.Maybe Kernel.External.Maps.Language),
    maxDistance :: B.C f (Kernel.Prelude.Maybe Kernel.Utils.Common.Centesimal),
    maxDistanceValue :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecDistance),
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    originStopCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    placeNameSource :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    recentLocationId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    returnTime :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    riderId :: B.C f Kernel.Prelude.Text,
    riderPreferredOption :: B.C f (Kernel.Prelude.Maybe Domain.Types.SearchRequest.RiderPreferredOption),
    roundTrip :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    routeCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    selectedPaymentMethodId :: B.C f (Kernel.Prelude.Maybe Kernel.External.Payment.Interface.Types.PaymentMethodId),
    startTime :: B.C f Kernel.Prelude.UTCTime,
    toLocationId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    totalRidesCount :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    validTill :: B.C f Kernel.Prelude.UTCTime,
    vehicleCategory :: B.C f (Kernel.Prelude.Maybe BecknV2.OnDemand.Enums.VehicleCategory)
  }
  deriving (Generic, B.Beamable)

instance B.Table SearchRequestT where
  data PrimaryKey SearchRequestT f = SearchRequestId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = SearchRequestId . id

type SearchRequest = SearchRequestT Identity

$(enableKVPG ''SearchRequestT ['id] [['riderId]])

$(mkTableInstances ''SearchRequestT "search_request")
