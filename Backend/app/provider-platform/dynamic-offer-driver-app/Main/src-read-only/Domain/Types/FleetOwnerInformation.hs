{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FleetOwnerInformation where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data FleetOwnerInformation = FleetOwnerInformation
  { blocked :: Kernel.Prelude.Bool,
    enabled :: Kernel.Prelude.Bool,
    fleetOwnerPersonId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    fleetType :: Domain.Types.FleetOwnerInformation.FleetType,
    gstImageId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    gstNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    verified :: Kernel.Prelude.Bool,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data FleetType = RENTAL_FLEET | NORMAL_FLEET deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''FleetType)
