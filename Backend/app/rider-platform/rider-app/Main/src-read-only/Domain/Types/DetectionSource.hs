{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.DetectionSource where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.ServiceCategory
import qualified Domain.Types.TicketService
import qualified Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data DetectionSource = DetectionSource
  { anomalyAlertsEnabled :: Kernel.Prelude.Bool,
    authCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    authMode :: Domain.Types.DetectionSource.DetectionAuthMode,
    authSecret :: Kernel.Prelude.Maybe (Kernel.External.Encryption.EncryptedField 'Kernel.External.Encryption.AsEncrypted Kernel.Prelude.Text),
    enabled :: Kernel.Prelude.Bool,
    enforcementEnabled :: Kernel.Prelude.Bool,
    enforcementGraceMinutes :: Kernel.Prelude.Int,
    id :: Kernel.Types.Id.Id Domain.Types.DetectionSource.DetectionSource,
    imagesEnabled :: Kernel.Prelude.Bool,
    mappings :: [Domain.Types.DetectionSource.DetectionMapping],
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    name :: Kernel.Prelude.Text,
    occupancyMode :: Domain.Types.DetectionSource.OccupancyMode,
    vendorKey :: Kernel.Prelude.Text,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data DetectionAuthMode = Hmac | Bearer | Custom deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data DetectionMapping = DetectionMapping
  { externalId :: Kernel.Prelude.Text,
    serviceCategoryId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory),
    ticketServiceId :: Kernel.Types.Id.Id Domain.Types.TicketService.TicketService
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Eq)

data OccupancyMode = Availability | Session deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''DetectionAuthMode)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''OccupancyMode)
