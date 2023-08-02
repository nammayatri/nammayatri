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

module Storage.Queries.DriverOnboarding.AadhaarVerification where

import Domain.Types.DriverOnboarding.AadhaarVerification
import Domain.Types.Person (Person)
import qualified EulerHS.Language as L
import Kernel.External.Encryption (DbHash)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Utils
import qualified Sequelize as Se
import qualified Storage.Beam.DriverOnboarding.AadhaarVerification as BeamAV

-- create :: AadhaarVerification -> Esq.SqlDB ()
-- create = Esq.create

create :: (L.MonadFlow m, Log m) => AadhaarVerification -> m ()
create = createWithKV

-- findById ::
--   Transactionable m =>
--   Id AadhaarVerification ->
--   m (Maybe AadhaarVerification)
-- findById = Esq.findById

-- findById :: (L.MonadFlow m, Log m) => Id AadhaarVerification -> m (Maybe AadhaarVerification)
-- findById (Id aadhaarVerification) = findOneWithKV [Se.Is BeamAV.id $ Se.Eq aadhaarVerification]

-- findByDriverId ::
--   Transactionable m =>
--   Id Person ->
--   m (Maybe AadhaarVerification)
-- findByDriverId driverId = do
--   findOne $ do
--     aadhaar <- from $ table @AadhaarVerificationT
--     where_ $ aadhaar ^. AadhaarVerificationDriverId ==. val (toKey driverId)
--     return aadhaar

findByDriverId :: (L.MonadFlow m, Log m) => Id Person -> m (Maybe AadhaarVerification)
findByDriverId (Id driverId) = findOneWithKV [Se.Is BeamAV.driverId $ Se.Eq driverId]

findByDriverIdInReplica :: (L.MonadFlow m, Log m) => Id Person -> m (Maybe AadhaarVerification)
findByDriverIdInReplica (Id driverId) = findOneWithKvInReplica [Se.Is BeamAV.driverId $ Se.Eq driverId]

-- deleteByDriverId :: Id Person -> SqlDB ()
-- deleteByDriverId driverId =
--   Esq.delete $ do
--     aadhaar <- from $ table @AadhaarVerificationT
--     where_ $ aadhaar ^. AadhaarVerificationDriverId ==. val (toKey driverId)

deleteByDriverId :: (L.MonadFlow m, Log m) => Id Person -> m ()
deleteByDriverId (Id driverId) = deleteWithKV [Se.Is BeamAV.driverId (Se.Eq driverId)]

-- findByAadhaarNumberHash ::
--   Transactionable m =>
--   DbHash ->
--   m (Maybe AadhaarVerification)
-- findByAadhaarNumberHash aadhaarHash = do
--   findOne $ do
--     aadhaar <- from $ table @AadhaarVerificationT
--     where_ $ aadhaar ^. AadhaarVerificationAadhaarNumberHash ==. val (Just aadhaarHash)
--     return aadhaar

findByAadhaarNumberHash :: (L.MonadFlow m, Log m) => DbHash -> m (Maybe AadhaarVerification)
findByAadhaarNumberHash aadhaarHash = findOneWithKV [Se.Is BeamAV.aadhaarNumberHash $ Se.Eq (Just aadhaarHash)]

findByPhoneNumberAndUpdate :: (L.MonadFlow m, Log m, MonadTime m) => Text -> Text -> Text -> Maybe DbHash -> Bool -> Id Person -> m ()
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

-- findByPhoneNumberAndUpdate :: Text -> Text -> Text -> Maybe DbHash -> Bool -> Id Person -> Esq.SqlDB ()
-- findByPhoneNumberAndUpdate name gender dob aadhaarNumberHash isVerified personId = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ AadhaarVerificationDriverName =. val name,
--         AadhaarVerificationDriverGender =. val gender,
--         AadhaarVerificationDriverDob =. val dob,
--         AadhaarVerificationAadhaarNumberHash =. val aadhaarNumberHash,
--         AadhaarVerificationIsVerified =. val isVerified,
--         AadhaarVerificationUpdatedAt =. val now
--       ]
--     where_ $ tbl ^. AadhaarVerificationDriverId ==. val (toKey personId)

-- deleteByPersonId :: Id Person -> SqlDB ()
-- deleteByPersonId personId =
--   Esq.delete $ do
--     verifications <- from $ table @AadhaarVerificationT
--     where_ $ verifications ^. AadhaarVerificationDriverId ==. val (toKey personId)

deleteByPersonId :: (L.MonadFlow m, Log m) => Id Person -> m ()
deleteByPersonId (Id personId) = deleteWithKV [Se.Is BeamAV.driverId (Se.Eq personId)]

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
            updatedAt = updatedAt
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
        BeamAV.updatedAt = updatedAt
      }
