{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.VehicleRegistrationCertificate where

import Data.Aeson
import qualified Domain.Types.IdfyVerification
import qualified Domain.Types.Image
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.Vehicle
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data VehicleRegistrationCertificateE e = VehicleRegistrationCertificate
  { airConditioned :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    certificateNumber :: Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text,
    documentImageId :: Kernel.Types.Id.Id Domain.Types.Image.Image,
    failedRules :: [Kernel.Prelude.Text],
    fitnessExpiry :: Kernel.Prelude.UTCTime,
    fleetOwnerId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate,
    insuranceValidity :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    luggageCapacity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    manufacturerModel :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    permitExpiry :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    pucExpiry :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    reviewRequired :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    reviewedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    userPassedVehicleCategory :: Kernel.Prelude.Maybe Domain.Types.Vehicle.Category,
    vehicleCapacity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    vehicleClass :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleColor :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleDoors :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    vehicleEnergyType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleManufacturer :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleModel :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleRating :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    vehicleSeatBelts :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    vehicleVariant :: Kernel.Prelude.Maybe Domain.Types.Vehicle.Variant,
    verificationStatus :: Domain.Types.IdfyVerification.VerificationStatus,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)

type VehicleRegistrationCertificate = VehicleRegistrationCertificateE 'AsEncrypted

type DecryptedVehicleRegistrationCertificate = VehicleRegistrationCertificateE 'AsUnencrypted

instance EncryptedItem VehicleRegistrationCertificate where
  type Unencrypted VehicleRegistrationCertificate = (DecryptedVehicleRegistrationCertificate, HashSalt)
  encryptItem (entity, salt) = do
    certificateNumber_ <- encryptItem (certificateNumber entity, salt)
    pure
      VehicleRegistrationCertificate
        { airConditioned = airConditioned entity,
          certificateNumber = certificateNumber_,
          documentImageId = documentImageId entity,
          failedRules = failedRules entity,
          fitnessExpiry = fitnessExpiry entity,
          fleetOwnerId = fleetOwnerId entity,
          id = id entity,
          insuranceValidity = insuranceValidity entity,
          luggageCapacity = luggageCapacity entity,
          manufacturerModel = manufacturerModel entity,
          permitExpiry = permitExpiry entity,
          pucExpiry = pucExpiry entity,
          reviewRequired = reviewRequired entity,
          reviewedAt = reviewedAt entity,
          userPassedVehicleCategory = userPassedVehicleCategory entity,
          vehicleCapacity = vehicleCapacity entity,
          vehicleClass = vehicleClass entity,
          vehicleColor = vehicleColor entity,
          vehicleDoors = vehicleDoors entity,
          vehicleEnergyType = vehicleEnergyType entity,
          vehicleManufacturer = vehicleManufacturer entity,
          vehicleModel = vehicleModel entity,
          vehicleRating = vehicleRating entity,
          vehicleSeatBelts = vehicleSeatBelts entity,
          vehicleVariant = vehicleVariant entity,
          verificationStatus = verificationStatus entity,
          merchantId = merchantId entity,
          merchantOperatingCityId = merchantOperatingCityId entity,
          createdAt = createdAt entity,
          updatedAt = updatedAt entity
        }
  decryptItem entity = do
    certificateNumber_ <- fst <$> decryptItem (certificateNumber entity)
    pure
      ( VehicleRegistrationCertificate
          { airConditioned = airConditioned entity,
            certificateNumber = certificateNumber_,
            documentImageId = documentImageId entity,
            failedRules = failedRules entity,
            fitnessExpiry = fitnessExpiry entity,
            fleetOwnerId = fleetOwnerId entity,
            id = id entity,
            insuranceValidity = insuranceValidity entity,
            luggageCapacity = luggageCapacity entity,
            manufacturerModel = manufacturerModel entity,
            permitExpiry = permitExpiry entity,
            pucExpiry = pucExpiry entity,
            reviewRequired = reviewRequired entity,
            reviewedAt = reviewedAt entity,
            userPassedVehicleCategory = userPassedVehicleCategory entity,
            vehicleCapacity = vehicleCapacity entity,
            vehicleClass = vehicleClass entity,
            vehicleColor = vehicleColor entity,
            vehicleDoors = vehicleDoors entity,
            vehicleEnergyType = vehicleEnergyType entity,
            vehicleManufacturer = vehicleManufacturer entity,
            vehicleModel = vehicleModel entity,
            vehicleRating = vehicleRating entity,
            vehicleSeatBelts = vehicleSeatBelts entity,
            vehicleVariant = vehicleVariant entity,
            verificationStatus = verificationStatus entity,
            merchantId = merchantId entity,
            merchantOperatingCityId = merchantOperatingCityId entity,
            createdAt = createdAt entity,
            updatedAt = updatedAt entity
          },
        ""
      )

instance EncryptedItem' VehicleRegistrationCertificate where
  type UnencryptedItem VehicleRegistrationCertificate = DecryptedVehicleRegistrationCertificate
  toUnencrypted a salt = (a, salt)
  fromUnencrypted = fst
