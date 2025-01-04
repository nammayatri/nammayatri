{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.VehicleRouteMapping where

import Data.Aeson
import qualified Data.Text
import qualified Domain.Types.Common
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
    routeCode :: Data.Text.Text,
    vehicleClass :: Data.Text.Text,
    vehicleColor :: Data.Text.Text,
    vehicleModel :: Data.Text.Text,
    vehicleNumber :: Kernel.External.Encryption.EncryptedHashedField e Data.Text.Text,
    vehicleServiceTierType :: Domain.Types.Common.ServiceTierType,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)

type VehicleRouteMapping = VehicleRouteMappingE ('AsEncrypted)

type DecryptedVehicleRouteMapping = VehicleRouteMappingE ('AsUnencrypted)

instance EncryptedItem VehicleRouteMapping where
  type Unencrypted VehicleRouteMapping = (DecryptedVehicleRouteMapping, HashSalt)
  encryptItem (entity, salt) = do
    vehicleNumber_ <- encryptItem (vehicleNumber entity, salt)
    pure
      VehicleRouteMapping
        { blocked = blocked entity,
          fleetOwnerId = fleetOwnerId entity,
          routeCode = routeCode entity,
          vehicleClass = vehicleClass entity,
          vehicleColor = vehicleColor entity,
          vehicleModel = vehicleModel entity,
          vehicleNumber = vehicleNumber_,
          vehicleServiceTierType = vehicleServiceTierType entity,
          merchantId = merchantId entity,
          merchantOperatingCityId = merchantOperatingCityId entity,
          createdAt = createdAt entity,
          updatedAt = updatedAt entity
        }
  decryptItem entity = do
    vehicleNumber_ <- fst <$> decryptItem (vehicleNumber entity)
    pure
      ( VehicleRouteMapping
          { blocked = blocked entity,
            fleetOwnerId = fleetOwnerId entity,
            routeCode = routeCode entity,
            vehicleClass = vehicleClass entity,
            vehicleColor = vehicleColor entity,
            vehicleModel = vehicleModel entity,
            vehicleNumber = vehicleNumber_,
            vehicleServiceTierType = vehicleServiceTierType entity,
            merchantId = merchantId entity,
            merchantOperatingCityId = merchantOperatingCityId entity,
            createdAt = createdAt entity,
            updatedAt = updatedAt entity
          },
        ""
      )

instance EncryptedItem' VehicleRouteMapping where
  type UnencryptedItem VehicleRouteMapping = DecryptedVehicleRouteMapping
  toUnencrypted a salt = (a, salt)
  fromUnencrypted = fst
