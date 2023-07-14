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
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Types.Logging (Log)
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

findById :: (L.MonadFlow m, Log m) => Id AadhaarVerification -> m (Maybe AadhaarVerification)
findById (Id aadhaarVerification) = findOneWithKV [Se.Is BeamAV.id $ Se.Eq aadhaarVerification]

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

-- deleteByDriverId :: Id Person -> SqlDB ()
-- deleteByDriverId driverId =
--   Esq.delete $ do
--     aadhaar <- from $ table @AadhaarVerificationT
--     where_ $ aadhaar ^. AadhaarVerificationDriverId ==. val (toKey driverId)

deleteByDriverId :: (L.MonadFlow m, Log m) => Id Person -> m ()
deleteByDriverId (Id driverId) = deleteWithKV [Se.Is BeamAV.driverId (Se.Eq driverId)]

instance FromTType' BeamAV.AadhaarVerification AadhaarVerification where
  fromTType' BeamAV.AadhaarVerificationT {..} = do
    pure $
      Just
        AadhaarVerification
          { id = Id id,
            driverId = Id driverId,
            driverName = driverName,
            driverGender = driverGender,
            driverDob = driverDob,
            driverImage = driverImage,
            createdAt = createdAt
          }

instance ToTType' BeamAV.AadhaarVerification AadhaarVerification where
  toTType' AadhaarVerification {..} = do
    BeamAV.AadhaarVerificationT
      { BeamAV.id = getId id,
        BeamAV.driverId = getId driverId,
        BeamAV.driverName = driverName,
        BeamAV.driverGender = driverGender,
        BeamAV.driverDob = driverDob,
        BeamAV.driverImage = driverImage,
        BeamAV.createdAt = createdAt
      }
