{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.SearchRequest where

import Data.Aeson
import qualified Domain.Types.Client
import qualified Domain.Types.Location
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.MerchantPaymentMethod
import qualified Domain.Types.Person
import qualified Kernel.External.Maps
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Types.Version
import Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data SearchRequest = SearchRequest
  { id :: Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest,
    startTime :: Kernel.Prelude.UTCTime,
    returnTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    roundTrip :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    validTill :: Kernel.Prelude.UTCTime,
    riderId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    clientId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Client.Client),
    fromLocation :: Domain.Types.Location.Location,
    toLocation :: Kernel.Prelude.Maybe Domain.Types.Location.Location,
    distance :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance,
    maxDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance,
    distanceUnit :: Kernel.Types.Common.DistanceUnit,
    estimatedRideDuration :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    device :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    language :: Kernel.Prelude.Maybe Kernel.External.Maps.Language,
    disabilityTag :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    customerExtraFee :: Kernel.Prelude.Maybe Kernel.Types.Common.Price,
    autoAssignEnabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    autoAssignEnabledV2 :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    availablePaymentMethods :: [Kernel.Types.Id.Id Domain.Types.MerchantPaymentMethod.MerchantPaymentMethod],
    selectedPaymentMethodId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantPaymentMethod.MerchantPaymentMethod),
    riderPreferredOption :: Domain.Types.SearchRequest.RiderPreferredOption,
    createdAt :: Kernel.Prelude.UTCTime,
    clientBundleVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    clientSdkVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    clientConfigVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    clientDevice :: Kernel.Prelude.Maybe Kernel.Types.Version.Device,
    backendConfigVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    backendAppVersion :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    isAdvanceBookingEnabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool
  }
  deriving (Generic, Show)

data RiderPreferredOption = Rental | OneWay | InterCity deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data SearchRequestStatus = NEW | INPROGRESS | CONFIRMED | COMPLETED | CLOSED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''RiderPreferredOption)

$(mkHttpInstancesForEnum ''RiderPreferredOption)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''SearchRequestStatus)

$(mkHttpInstancesForEnum ''SearchRequestStatus)
