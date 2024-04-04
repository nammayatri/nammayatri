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

deleteByDriverId :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
deleteByDriverId (Kernel.Types.Id.Id driverId) = do deleteWithKV [Se.Is Beam.driverId $ Se.Eq driverId]

findByAadhaarNumberHash :: KvDbFlow m r => (Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash -> m (Maybe Domain.Types.AadhaarVerification.AadhaarVerification))
findByAadhaarNumberHash aadhaarNumberHash = do findOneWithKV [Se.Is Beam.aadhaarNumberHash $ Se.Eq aadhaarNumberHash]

findByDriverId :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.AadhaarVerification.AadhaarVerification))
findByDriverId (Kernel.Types.Id.Id driverId) = do findOneWithKV [Se.Is Beam.driverId $ Se.Eq driverId]

findByPhoneNumberAndUpdate ::
  KvDbFlow m r =>
  (Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash -> Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
findByPhoneNumberAndUpdate driverName driverGender driverDob aadhaarNumberHash isVerified (Kernel.Types.Id.Id driverId) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.driverName driverName,
      Se.Set Beam.driverGender driverGender,
      Se.Set Beam.driverDob driverDob,
      Se.Set Beam.aadhaarNumberHash aadhaarNumberHash,
      Se.Set Beam.isVerified isVerified,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.driverId $ Se.Eq driverId]

updateDriverImagePath :: KvDbFlow m r => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateDriverImagePath driverImagePath (Kernel.Types.Id.Id driverId) = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.driverImagePath driverImagePath, Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq driverId]

findByPrimaryKey :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.AadhaarVerification.AadhaarVerification))
findByPrimaryKey (Kernel.Types.Id.Id driverId) = do findOneWithKV [Se.And [Se.Is Beam.driverId $ Se.Eq driverId]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.AadhaarVerification.AadhaarVerification -> m ())
updateByPrimaryKey (Domain.Types.AadhaarVerification.AadhaarVerification {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.aadhaarNumberHash aadhaarNumberHash,
      Se.Set Beam.driverDob driverDob,
      Se.Set Beam.driverGender driverGender,
      Se.Set Beam.driverImage driverImage,
      Se.Set Beam.driverImagePath driverImagePath,
      Se.Set Beam.driverName driverName,
      Se.Set Beam.isVerified isVerified,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]]

instance FromTType' Beam.AadhaarVerification Domain.Types.AadhaarVerification.AadhaarVerification where
  fromTType' (Beam.AadhaarVerificationT {..}) = do
    pure $
      Just
        Domain.Types.AadhaarVerification.AadhaarVerification
          { aadhaarNumberHash = aadhaarNumberHash,
            driverDob = driverDob,
            driverGender = driverGender,
            driverId = Kernel.Types.Id.Id driverId,
            driverImage = driverImage,
            driverImagePath = driverImagePath,
            driverName = driverName,
            isVerified = isVerified,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.AadhaarVerification Domain.Types.AadhaarVerification.AadhaarVerification where
  toTType' (Domain.Types.AadhaarVerification.AadhaarVerification {..}) = do
    Beam.AadhaarVerificationT
      { Beam.aadhaarNumberHash = aadhaarNumberHash,
        Beam.driverDob = driverDob,
        Beam.driverGender = driverGender,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.driverImage = driverImage,
        Beam.driverImagePath = driverImagePath,
        Beam.driverName = driverName,
        Beam.isVerified = isVerified,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
