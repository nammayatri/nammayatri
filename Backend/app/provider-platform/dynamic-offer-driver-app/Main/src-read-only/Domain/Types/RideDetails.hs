{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.RideDetails where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Ride
import qualified Domain.Types.VehicleVariant
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Types.Time
import qualified Tools.Beam.UtilsTH

data RideDetailsE e = RideDetails
  { createdAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    defaultServiceTierName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverCountryCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverName :: Kernel.Prelude.Text,
    driverNumber :: Kernel.Prelude.Maybe (Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text),
    fleetOwnerId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.Ride.Ride,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    rcId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleAge :: Kernel.Prelude.Maybe Kernel.Types.Time.Months,
    vehicleClass :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleColor :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleModel :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleNumber :: Kernel.Prelude.Text,
    vehicleVariant :: Kernel.Prelude.Maybe Domain.Types.VehicleVariant.VehicleVariant
  }
  deriving (Generic)

type RideDetails = RideDetailsE 'AsEncrypted

type DecryptedRideDetails = RideDetailsE 'AsUnencrypted

instance EncryptedItem RideDetails where
  type Unencrypted RideDetails = (DecryptedRideDetails, HashSalt)
  encryptItem (entity, salt) = do
    driverNumber_ <- encryptItem $ (,salt) <$> driverNumber entity
    pure
      RideDetails
        { createdAt = createdAt entity,
          defaultServiceTierName = defaultServiceTierName entity,
          driverCountryCode = driverCountryCode entity,
          driverName = driverName entity,
          driverNumber = driverNumber_,
          fleetOwnerId = fleetOwnerId entity,
          id = id entity,
          merchantId = merchantId entity,
          merchantOperatingCityId = merchantOperatingCityId entity,
          rcId = rcId entity,
          vehicleAge = vehicleAge entity,
          vehicleClass = vehicleClass entity,
          vehicleColor = vehicleColor entity,
          vehicleModel = vehicleModel entity,
          vehicleNumber = vehicleNumber entity,
          vehicleVariant = vehicleVariant entity
        }
  decryptItem entity = do
    driverNumber_ <- fmap fst <$> decryptItem (driverNumber entity)
    pure
      ( RideDetails
          { createdAt = createdAt entity,
            defaultServiceTierName = defaultServiceTierName entity,
            driverCountryCode = driverCountryCode entity,
            driverName = driverName entity,
            driverNumber = driverNumber_,
            fleetOwnerId = fleetOwnerId entity,
            id = id entity,
            merchantId = merchantId entity,
            merchantOperatingCityId = merchantOperatingCityId entity,
            rcId = rcId entity,
            vehicleAge = vehicleAge entity,
            vehicleClass = vehicleClass entity,
            vehicleColor = vehicleColor entity,
            vehicleModel = vehicleModel entity,
            vehicleNumber = vehicleNumber entity,
            vehicleVariant = vehicleVariant entity
          },
        ""
      )

instance EncryptedItem' RideDetails where
  type UnencryptedItem RideDetails = DecryptedRideDetails
  toUnencrypted a salt = (a, salt)
  fromUnencrypted = fst
