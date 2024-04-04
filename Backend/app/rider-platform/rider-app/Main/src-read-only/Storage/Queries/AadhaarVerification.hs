{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.AadhaarVerification where

import qualified Domain.Types.AadhaarVerification
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.AadhaarVerification as Beam

create :: KvDbFlow m r => (Domain.Types.AadhaarVerification.AadhaarVerification -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.AadhaarVerification.AadhaarVerification] -> m ())
createMany = traverse_ create

deleteByPersonId :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
deleteByPersonId (Kernel.Types.Id.Id personId) = do deleteWithKV [Se.Is Beam.personId $ Se.Eq personId]

findByAadhaarNumberHash :: KvDbFlow m r => (Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash -> m (Maybe Domain.Types.AadhaarVerification.AadhaarVerification))
findByAadhaarNumberHash aadhaarNumberHash = do findOneWithKV [Se.Is Beam.aadhaarNumberHash $ Se.Eq aadhaarNumberHash]

findByPersonId :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.AadhaarVerification.AadhaarVerification))
findByPersonId (Kernel.Types.Id.Id personId) = do findOneWithKV [Se.Is Beam.personId $ Se.Eq personId]

findByPhoneNumberAndUpdate ::
  KvDbFlow m r =>
  (Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash -> Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
findByPhoneNumberAndUpdate personName personGender personDob aadhaarNumberHash isVerified (Kernel.Types.Id.Id personId) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.personName personName,
      Se.Set Beam.personGender personGender,
      Se.Set Beam.personDob personDob,
      Se.Set Beam.aadhaarNumberHash aadhaarNumberHash,
      Se.Set Beam.isVerified isVerified,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.personId $ Se.Eq personId]

updatePersonImagePath :: KvDbFlow m r => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updatePersonImagePath personImagePath (Kernel.Types.Id.Id personId) = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.personImagePath personImagePath, Se.Set Beam.updatedAt _now] [Se.Is Beam.personId $ Se.Eq personId]

findByPrimaryKey :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.AadhaarVerification.AadhaarVerification))
findByPrimaryKey (Kernel.Types.Id.Id personId) = do findOneWithKV [Se.And [Se.Is Beam.personId $ Se.Eq personId]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.AadhaarVerification.AadhaarVerification -> m ())
updateByPrimaryKey (Domain.Types.AadhaarVerification.AadhaarVerification {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.aadhaarNumberHash aadhaarNumberHash,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.isVerified isVerified,
      Se.Set Beam.personDob personDob,
      Se.Set Beam.personGender personGender,
      Se.Set Beam.personImagePath personImagePath,
      Se.Set Beam.personName personName,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]]

instance FromTType' Beam.AadhaarVerification Domain.Types.AadhaarVerification.AadhaarVerification where
  fromTType' (Beam.AadhaarVerificationT {..}) = do
    pure $
      Just
        Domain.Types.AadhaarVerification.AadhaarVerification
          { aadhaarNumberHash = aadhaarNumberHash,
            createdAt = createdAt,
            isVerified = isVerified,
            personDob = personDob,
            personGender = personGender,
            personId = Kernel.Types.Id.Id personId,
            personImagePath = personImagePath,
            personName = personName,
            updatedAt = updatedAt
          }

instance ToTType' Beam.AadhaarVerification Domain.Types.AadhaarVerification.AadhaarVerification where
  toTType' (Domain.Types.AadhaarVerification.AadhaarVerification {..}) = do
    Beam.AadhaarVerificationT
      { Beam.aadhaarNumberHash = aadhaarNumberHash,
        Beam.createdAt = createdAt,
        Beam.isVerified = isVerified,
        Beam.personDob = personDob,
        Beam.personGender = personGender,
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.personImagePath = personImagePath,
        Beam.personName = personName,
        Beam.updatedAt = updatedAt
      }
