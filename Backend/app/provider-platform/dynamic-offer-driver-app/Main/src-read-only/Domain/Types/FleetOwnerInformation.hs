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
  { aadhaarBackImageId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    aadhaarFrontImageId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    aadhaarNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    blocked :: Kernel.Prelude.Bool,
    businessLicenseImageId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    businessLicenseNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    enabled :: Kernel.Prelude.Bool,
    fleetOwnerPersonId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    fleetType :: Domain.Types.FleetOwnerInformation.FleetType,
    gstImageId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    gstNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    panImageId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    panNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    referredByOperatorId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    registeredAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    verified :: Kernel.Prelude.Bool,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data FleetType = RENTAL_FLEET | NORMAL_FLEET | BUSINESS_FLEET deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''FleetType)
