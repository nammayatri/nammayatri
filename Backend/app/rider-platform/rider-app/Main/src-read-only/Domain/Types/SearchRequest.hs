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
  { autoAssignEnabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    autoAssignEnabledV2 :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    availablePaymentMethods :: [Kernel.Types.Id.Id Domain.Types.MerchantPaymentMethod.MerchantPaymentMethod],
    backendAppVersion :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    backendConfigVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    clientBundleVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    clientConfigVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    clientDevice :: Kernel.Prelude.Maybe Kernel.Types.Version.Device,
    clientId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Client.Client),
    clientSdkVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    createdAt :: Kernel.Prelude.UTCTime,
    customerExtraFee :: Kernel.Prelude.Maybe Kernel.Types.Common.Price,
    device :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    disabilityTag :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    distance :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance,
    estimatedRideDuration :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    fromLocation :: Domain.Types.Location.Location,
    id :: Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest,
    language :: Kernel.Prelude.Maybe Kernel.External.Maps.Language,
    maxDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    riderId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    riderPreferredOption :: Domain.Types.SearchRequest.RiderPreferredOption,
    selectedPaymentMethodId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantPaymentMethod.MerchantPaymentMethod),
    startTime :: Kernel.Prelude.UTCTime,
    toLocation :: Kernel.Prelude.Maybe Domain.Types.Location.Location,
    validTill :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show)

data RiderPreferredOption = Rental | OneWay deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data SearchRequestStatus = NEW | INPROGRESS | CONFIRMED | COMPLETED | CLOSED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''RiderPreferredOption)

$(mkHttpInstancesForEnum ''RiderPreferredOption)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''SearchRequestStatus)

$(mkHttpInstancesForEnum ''SearchRequestStatus)
