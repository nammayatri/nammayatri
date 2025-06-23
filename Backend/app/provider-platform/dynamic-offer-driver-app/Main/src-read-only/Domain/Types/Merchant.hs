{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Merchant where

import Data.Aeson
import qualified Domain.Types
import Domain.Types.Common (UsageSafety (..))
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Geofencing
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data MerchantD (s :: UsageSafety) = Merchant
  { city :: Kernel.Types.Beckn.Context.City,
    country :: Kernel.Types.Beckn.Context.Country,
    createdAt :: Kernel.Prelude.UTCTime,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    enabled :: Kernel.Prelude.Bool,
    fleetOwnerEnabledCheck :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    fromTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    gatewayAndRegistryPriorityList :: [Domain.Types.GatewayAndRegistryService],
    geoHashPrecisionValue :: Kernel.Prelude.Int,
    geofencingConfig :: Kernel.Types.Geofencing.GeofencingConfig,
    gstin :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    headCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    id :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    info :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    internalApiKey :: Kernel.Prelude.Text,
    minimumDriverRatesCount :: Kernel.Prelude.Int,
    mobileCountryCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    mobileNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    name :: Kernel.Prelude.Text,
    onlinePayment :: Kernel.Prelude.Bool,
    overwriteAssociation :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    registryUrl :: Kernel.Prelude.BaseUrl,
    shortId :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant,
    state :: Kernel.Types.Beckn.Context.IndianState,
    status :: Domain.Types.Merchant.Status,
    subscriberId :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Subscriber,
    toTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    uniqueKeyId :: Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime,
    verified :: Kernel.Prelude.Bool
  }
  deriving (Generic, Show)

data MerchantAPIEntity = MerchantAPIEntity
  { contactNumber :: Kernel.Prelude.Text,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    enabled :: Kernel.Prelude.Bool,
    id :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    name :: Kernel.Prelude.Text,
    status :: Domain.Types.Merchant.Status
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data Status = PENDING_VERIFICATION | APPROVED | REJECTED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data Subscriber = Subscriber {} deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

type Merchant = MerchantD ('Safe)

instance FromJSON (MerchantD 'Unsafe)

instance ToJSON (MerchantD 'Unsafe)

instance FromJSON (MerchantD 'Safe)

instance ToJSON (MerchantD 'Safe)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''Status))

$(mkHttpInstancesForEnum (''Status))
