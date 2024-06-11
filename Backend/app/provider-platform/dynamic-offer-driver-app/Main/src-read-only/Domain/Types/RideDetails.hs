{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.RideDetails where

import Data.Aeson
import qualified Domain.Types.Ride
import qualified Domain.Types.Vehicle
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data RideDetailsE e = RideDetails
  { createdAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    defaultServiceTierName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverCountryCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverName :: Kernel.Prelude.Text,
    driverNumber :: Kernel.Prelude.Maybe (Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text),
    fleetOwnerId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.Ride.Ride,
    vehicleClass :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleColor :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleModel :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleNumber :: Kernel.Prelude.Text,
    vehicleVariant :: Kernel.Prelude.Maybe Domain.Types.Vehicle.Variant
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

{-
	DSL Source Link: file://./../../../spec/Storage/RideDetails.yaml
-}
