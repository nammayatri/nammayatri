{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.TDSDistributionRecord where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data TDSDistributionRecord = TDSDistributionRecord
  { assessmentYear :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    driverId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    emailAddress :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fileName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.TDSDistributionRecord.TDSDistributionRecord,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    quarter :: Kernel.Prelude.Text,
    retryCount :: Kernel.Prelude.Int,
    status :: Domain.Types.TDSDistributionRecord.TDSDistributionStatus,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data TDSDistributionStatus = PENDING | SENT | FAILED | MISSING_FILE | MISSING_MANIFEST | MISMATCH deriving (Show, (Eq), (Ord), (Read), (Generic), (ToJSON), (FromJSON), (ToSchema))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum (''TDSDistributionStatus))

$(Kernel.Utils.TH.mkFromHttpInstanceForEnum (''TDSDistributionStatus))
