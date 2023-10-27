{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.DriverOnboarding.AadhaarVerification where

import Domain.Types.DriverOnboarding.AadhaarVerification
import Domain.Types.Person (Person)
import Kernel.Beam.Functions
import Kernel.External.Encryption (DbHash)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.DriverOnboarding.AadhaarVerification as BeamAV

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => AadhaarVerification -> m ()
create = createWithKV

findByDriverId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> m (Maybe AadhaarVerification)
findByDriverId (Id driverId) = findOneWithKV [Se.Is BeamAV.driverId $ Se.Eq driverId]

deleteByDriverId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> m ()
deleteByDriverId (Id driverId) = deleteWithKV [Se.Is BeamAV.driverId (Se.Eq driverId)]

findByAadhaarNumberHash :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => DbHash -> m (Maybe AadhaarVerification)
findByAadhaarNumberHash aadhaarHash = findOneWithKV [Se.Is BeamAV.aadhaarNumberHash $ Se.Eq (Just aadhaarHash)]

findByPhoneNumberAndUpdate :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> Text -> Text -> Maybe DbHash -> Bool -> Id Person -> m ()
findByPhoneNumberAndUpdate name gender dob aadhaarNumberHash isVerified personId = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamAV.driverName name,
      Se.Set BeamAV.driverGender gender,
      Se.Set BeamAV.driverDob dob,
      Se.Set BeamAV.aadhaarNumberHash aadhaarNumberHash,
      Se.Set BeamAV.isVerified isVerified,
      Se.Set BeamAV.updatedAt now
    ]
    [Se.Is BeamAV.driverId (Se.Eq $ getId personId)]

deleteByPersonId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> m ()
deleteByPersonId (Id personId) = deleteWithKV [Se.Is BeamAV.driverId (Se.Eq personId)]

updateDriverImagePath :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> Text -> m ()
updateDriverImagePath (Id personId) imagePath =
  updateOneWithKV
    [Se.Set BeamAV.driverImagePath (Just imagePath)]
    [Se.Is BeamAV.driverId (Se.Eq personId)]

instance FromTType' BeamAV.AadhaarVerification AadhaarVerification where
  fromTType' BeamAV.AadhaarVerificationT {..} = do
    pure $
      Just
        AadhaarVerification
          { driverId = Id driverId,
            driverName = driverName,
            driverGender = driverGender,
            aadhaarNumberHash = aadhaarNumberHash,
            driverDob = driverDob,
            driverImage = driverImage,
            isVerified = isVerified,
            createdAt = createdAt,
            updatedAt = updatedAt,
            driverImagePath = driverImagePath
          }

instance ToTType' BeamAV.AadhaarVerification AadhaarVerification where
  toTType' AadhaarVerification {..} = do
    BeamAV.AadhaarVerificationT
      { BeamAV.driverId = getId driverId,
        BeamAV.driverName = driverName,
        BeamAV.driverGender = driverGender,
        BeamAV.aadhaarNumberHash = aadhaarNumberHash,
        BeamAV.driverDob = driverDob,
        BeamAV.driverImage = driverImage,
        BeamAV.isVerified = isVerified,
        BeamAV.createdAt = createdAt,
        BeamAV.updatedAt = updatedAt,
        BeamAV.driverImagePath = driverImagePath
      }
