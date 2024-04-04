{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DriverLicenseExtra where

import Domain.Types.DriverLicense
import Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverLicense as BeamDL
import Storage.Queries.OrphanInstances.DriverLicense

-- Extra code goes here --
upsert :: KvDbFlow m r => DriverLicense -> m ()
upsert a@DriverLicense {..} = do
  res <- findOneWithKV [Se.Is BeamDL.licenseNumberHash $ Se.Eq (a.licenseNumber & (.hash))]
  if isJust res
    then
      updateOneWithKV
        [ Se.Set BeamDL.driverDob driverDob,
          Se.Set BeamDL.driverName driverName,
          Se.Set BeamDL.licenseExpiry licenseExpiry,
          Se.Set BeamDL.classOfVehicles classOfVehicles,
          Se.Set BeamDL.verificationStatus verificationStatus,
          Se.Set BeamDL.failedRules failedRules,
          Se.Set BeamDL.updatedAt updatedAt
        ]
        [Se.Is BeamDL.licenseNumberHash $ Se.Eq (a.licenseNumber & (.hash))]
    else createWithKV a

findByDLNumber :: (KvDbFlow m r, EncFlow m r) => Text -> m (Maybe DriverLicense)
findByDLNumber dlNumber = do
  dlNumberHash <- getDbHash dlNumber
  findOneWithKV [Se.Is BeamDL.licenseNumberHash $ Se.Eq dlNumberHash]
