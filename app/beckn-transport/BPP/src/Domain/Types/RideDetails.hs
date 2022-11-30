{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.RideDetails where

import Beckn.External.Encryption
import Beckn.Types.Id
import qualified Domain.Types.Ride as SR
import qualified Domain.Types.Vehicle as SV
import EulerHS.Prelude hiding (id)
import Beckn.Types.Common

data RideDetailsE e = RideDetails
  { id :: Id SR.Ride,
    driverName :: Text,
    driverNumber :: Maybe (EncryptedHashedField e Text),
    driverCountryCode :: Maybe Text,
    vehicleNumber :: Text,
    vehicleColor :: Maybe Text,
    vehicleVariant :: Maybe SV.Variant,
    vehicleModel :: Maybe Text,
    vehicleClass :: Maybe Text
  }
  deriving (Generic)

type RideDetails = RideDetailsE 'AsEncrypted

type RideDetailsDecrypted = RideDetailsE 'AsUnencrypted

instance EncryptedItem RideDetails where
  type Unencrypted RideDetails = (RideDetailsDecrypted, HashSalt)
  encryptItem (RideDetails {..}, salt) = do
    driverNumber_ <- encryptItem $ (,salt) <$> driverNumber
    return RideDetails {driverNumber = driverNumber_, ..}
  decryptItem RideDetails {..} = do
    driverNumber_ <- fmap fst <$> decryptItem driverNumber
    return (RideDetails {driverNumber = driverNumber_, ..}, "")

instance EncryptedItem' RideDetails where
  type UnencryptedItem RideDetails = RideDetailsDecrypted
  toUnencrypted a salt = (a, salt)
  fromUnencrypted a = fst a

getDriverNumber :: (EsqDBFlow m r, EncFlow m r) => RideDetails -> m (Maybe Text)
getDriverNumber rideDetails = do
  decMobileNumber <- mapM decrypt rideDetails.driverNumber
  return $ rideDetails.driverCountryCode <> decMobileNumber