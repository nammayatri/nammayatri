{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.VehicleRegistrationCertificate where

import qualified Database.Beam as B
import qualified Domain.Types.IdfyVerification
import qualified Domain.Types.Vehicle
import Kernel.External.Encryption
import qualified Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data VehicleRegistrationCertificateT f = VehicleRegistrationCertificateT
  { airConditioned :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    certificateNumberEncrypted :: (B.C f Kernel.Prelude.Text),
    certificateNumberHash :: (B.C f Kernel.External.Encryption.DbHash),
    documentImageId :: (B.C f Kernel.Prelude.Text),
    failedRules :: (B.C f [Kernel.Prelude.Text]),
    fitnessExpiry :: (B.C f Kernel.Prelude.UTCTime),
    fleetOwnerId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    id :: (B.C f Kernel.Prelude.Text),
    insuranceValidity :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    luggageCapacity :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    manufacturerModel :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    permitExpiry :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    pucExpiry :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    reviewRequired :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    reviewedAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    userPassedVehicleCategory :: (B.C f (Kernel.Prelude.Maybe Domain.Types.Vehicle.Category)),
    vehicleCapacity :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    vehicleClass :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    vehicleColor :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    vehicleEnergyType :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    vehicleManufacturer :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    vehicleModel :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    vehicleRating :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
    vehicleVariant :: (B.C f (Kernel.Prelude.Maybe Domain.Types.Vehicle.Variant)),
    verificationStatus :: (B.C f Domain.Types.IdfyVerification.VerificationStatus),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table VehicleRegistrationCertificateT where
  data PrimaryKey VehicleRegistrationCertificateT f = VehicleRegistrationCertificateId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = VehicleRegistrationCertificateId . id

type VehicleRegistrationCertificate = VehicleRegistrationCertificateT Identity

$(enableKVPG (''VehicleRegistrationCertificateT) [('id)] [[('certificateNumberHash)]])

$(mkTableInstances (''VehicleRegistrationCertificateT) "vehicle_registration_certificate")
