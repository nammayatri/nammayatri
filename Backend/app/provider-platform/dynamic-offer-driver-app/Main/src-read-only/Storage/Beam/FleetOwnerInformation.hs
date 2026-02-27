{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FleetOwnerInformation where

import qualified Data.Aeson
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.FleetOwnerInformation
import Kernel.External.Encryption
import qualified Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data FleetOwnerInformationT f = FleetOwnerInformationT
  { aadhaarBackImageId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    aadhaarFrontImageId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    aadhaarNumberEncrypted :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    aadhaarNumberHash :: B.C f (Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash),
    aadhaarNumber :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    blocked :: B.C f Kernel.Prelude.Bool,
    businessLicenseImageId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    businessLicenseNumberEncrypted :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    businessLicenseNumberHash :: B.C f (Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash),
    businessLicenseNumber :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    enabled :: B.C f Kernel.Prelude.Bool,
    fleetDob :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    fleetName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    fleetOwnerPersonId :: B.C f Kernel.Prelude.Text,
    fleetType :: B.C f Domain.Types.FleetOwnerInformation.FleetType,
    gstImageId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    gstNumberEncrypted :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    gstNumberHash :: B.C f (Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash),
    gstNumber :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    isEligibleForSubscription :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    panImageId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    panNumberEncrypted :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    panNumberHash :: B.C f (Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash),
    panNumber :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    referredByOperatorId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    registeredAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    stripeAddress :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    stripeIdNumberEncrypted :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    stripeIdNumberHash :: B.C f (Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash),
    tdsRate :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    ticketPlaceId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    verified :: B.C f Kernel.Prelude.Bool,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table FleetOwnerInformationT where
  data PrimaryKey FleetOwnerInformationT f = FleetOwnerInformationId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FleetOwnerInformationId . fleetOwnerPersonId

type FleetOwnerInformation = FleetOwnerInformationT Identity

$(enableKVPG ''FleetOwnerInformationT ['fleetOwnerPersonId] [])

$(mkTableInstances ''FleetOwnerInformationT "fleet_owner_information")
