{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.VehicleRouteMapping where

import Data.Aeson
import qualified Data.Text
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data VehicleRouteMappingE e = VehicleRouteMapping
  { blocked :: Kernel.Prelude.Bool,
    fleetOwnerId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    routeCode :: Data.Text.Text,
    vehicleNumber :: Kernel.External.Encryption.EncryptedHashedField e Data.Text.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)

type VehicleRouteMapping = VehicleRouteMappingE 'AsEncrypted

type DecryptedVehicleRouteMapping = VehicleRouteMappingE 'AsUnencrypted

instance EncryptedItem VehicleRouteMapping where
  type Unencrypted VehicleRouteMapping = (DecryptedVehicleRouteMapping, HashSalt)
  encryptItem (entity, salt) = do
    vehicleNumber_ <- encryptItem (vehicleNumber entity, salt)
    pure
      VehicleRouteMapping
        { blocked = blocked entity,
          fleetOwnerId = fleetOwnerId entity,
          merchantId = merchantId entity,
          merchantOperatingCityId = merchantOperatingCityId entity,
          routeCode = routeCode entity,
          vehicleNumber = vehicleNumber_,
          createdAt = createdAt entity,
          updatedAt = updatedAt entity
        }
  decryptItem entity = do
    vehicleNumber_ <- fst <$> decryptItem (vehicleNumber entity)
    pure
      ( VehicleRouteMapping
          { blocked = blocked entity,
            fleetOwnerId = fleetOwnerId entity,
            merchantId = merchantId entity,
            merchantOperatingCityId = merchantOperatingCityId entity,
            routeCode = routeCode entity,
            vehicleNumber = vehicleNumber_,
            createdAt = createdAt entity,
            updatedAt = updatedAt entity
          },
        ""
      )

instance EncryptedItem' VehicleRouteMapping where
  type UnencryptedItem VehicleRouteMapping = DecryptedVehicleRouteMapping
  toUnencrypted a salt = (a, salt)
  fromUnencrypted = fst
