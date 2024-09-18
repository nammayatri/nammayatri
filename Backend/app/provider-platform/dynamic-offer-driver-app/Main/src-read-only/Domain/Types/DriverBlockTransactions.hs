{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.DriverBlockTransactions where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data DriverBlockTransactions = DriverBlockTransactions
  { blockLiftTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    blockReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    blockTimeInHours :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    blockedBy :: Domain.Types.DriverBlockTransactions.BlockedBy,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    id :: Kernel.Types.Id.Id Domain.Types.DriverBlockTransactions.DriverBlockTransactions,
    reasonCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    reportedAt :: Kernel.Prelude.UTCTime,
    requestorId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data BlockedBy = Dashboard | Application deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''BlockedBy)
