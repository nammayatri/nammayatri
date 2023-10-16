{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE ApplicativeDo #-}

module Domain.Types.RideDetails where

import qualified Domain.Types.Ride as SR
import qualified Domain.Types.Vehicle as SV
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption
import Kernel.Types.Id

data RideDetailsE e = RideDetails
  { id :: Id SR.Ride,
    driverName :: Text,
    driverNumber :: Maybe (EncryptedHashedField e Text),
    driverCountryCode :: Maybe Text,
    vehicleNumber :: Text,
    vehicleColor :: Maybe Text,
    vehicleVariant :: Maybe SV.Variant,
    vehicleModel :: Maybe Text,
    vehicleClass :: Maybe Text,
    fleetOwnerId :: Maybe Text
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

getDriverNumber :: EncFlow m r => RideDetails -> m (Maybe Text)
getDriverNumber rideDetails = do
  decMobileNumber <- mapM decrypt rideDetails.driverNumber
  return $ rideDetails.driverCountryCode <> decMobileNumber
