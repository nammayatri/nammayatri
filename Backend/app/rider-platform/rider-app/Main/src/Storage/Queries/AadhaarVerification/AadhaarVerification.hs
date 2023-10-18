{-# OPTIONS_GHC -Wno-missing-fields #-}
{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.AadhaarVerification.AadhaarVerification where

import Domain.Types.AadhaarVerification.AadhaarVerification
import Domain.Types.Person (Person)
import Kernel.Beam.Functions
import Kernel.External.Encryption (DbHash)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.AadhaarVerification.AadhaarVerification as BeamAV

create :: MonadFlow m => AadhaarVerification -> m ()
create = createWithKV

findByPersonId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> m (Maybe AadhaarVerification)
findByPersonId (Id personId) = findOneWithKV [Se.Is BeamAV.personId $ Se.Eq personId]

findByAadhaarNumberHash :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DbHash -> m (Maybe AadhaarVerification)
findByAadhaarNumberHash aadhaarHash = findOneWithKV [Se.Is BeamAV.aadhaarNumberHash $ Se.Eq (Just aadhaarHash)]

findByPhoneNumberAndUpdate :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> Text -> Text -> Maybe DbHash -> Bool -> Id Person -> m ()
findByPhoneNumberAndUpdate name gender dob aadhaarNumberHash isVerified personId = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamAV.personName name,
      Se.Set BeamAV.personGender gender,
      Se.Set BeamAV.personDob dob,
      Se.Set BeamAV.aadhaarNumberHash aadhaarNumberHash,
      Se.Set BeamAV.isVerified isVerified,
      Se.Set BeamAV.updatedAt now
    ]
    [Se.Is BeamAV.personId (Se.Eq $ getId personId)]

deleteByPersonId :: MonadFlow m => Id Person -> m ()
deleteByPersonId (Id personId) = deleteWithKV [Se.Is BeamAV.personId (Se.Eq personId)]

updatePersonImagePath :: MonadFlow m => Id Person -> Text -> m ()
updatePersonImagePath (Id personId) imagePath =
  updateOneWithKV
    [Se.Set BeamAV.personImagePath (Just imagePath)]
    [Se.Is BeamAV.personId (Se.Eq personId)]

instance FromTType' BeamAV.AadhaarVerification AadhaarVerification where
  fromTType' BeamAV.AadhaarVerificationT {..} = do
    pure $
      Just
        AadhaarVerification
          { personId = Id personId,
            personName = personName,
            personGender = personGender,
            aadhaarNumberHash = aadhaarNumberHash,
            personImagePath = personImagePath,
            personDob = personDob,
            isVerified = isVerified,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' BeamAV.AadhaarVerification AadhaarVerification where
  toTType' AadhaarVerification {..} = do
    BeamAV.AadhaarVerificationT
      { BeamAV.personId = getId personId,
        BeamAV.personName = personName,
        BeamAV.personGender = personGender,
        BeamAV.aadhaarNumberHash = aadhaarNumberHash,
        BeamAV.personDob = personDob,
        BeamAV.personImagePath = personImagePath,
        BeamAV.isVerified = isVerified,
        BeamAV.createdAt = createdAt,
        BeamAV.updatedAt = updatedAt
      }
