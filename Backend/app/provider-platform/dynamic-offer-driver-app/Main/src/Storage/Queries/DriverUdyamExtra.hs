module Storage.Queries.DriverUdyamExtra where

import Domain.Types.DriverUdyam
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Documents as Documents
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.DriverUdyam as Beam
import Storage.Queries.DriverUdyam ()

findAllByEncryptedUdyamNumber :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => DbHash -> m [DriverUdyam]
findAllByEncryptedUdyamNumber udyamNumberHash = do
  findAllWithKV [Se.Is Beam.udyamNumberHash $ Se.Eq udyamNumberHash]

findValidUdyamByHashExcludingDriver :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => DbHash -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe DriverUdyam)
findValidUdyamByHashExcludingDriver udyamNumberHash driverId =
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.udyamNumberHash $ Se.Eq udyamNumberHash,
          Se.Is Beam.driverId $ Se.Not $ Se.Eq (Kernel.Types.Id.getId driverId),
          Se.Is Beam.verificationStatus $ Se.Eq Documents.VALID
        ]
    ]

findValidUdyamByHash :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => DbHash -> m (Maybe DriverUdyam)
findValidUdyamByHash udyamNumberHash =
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.udyamNumberHash $ Se.Eq udyamNumberHash,
          Se.Is Beam.verificationStatus $ Se.Eq Documents.VALID
        ]
    ]

findValidUdyamByDriverId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe DriverUdyam)
findValidUdyamByDriverId driverId =
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId),
          Se.Is Beam.verificationStatus $ Se.Eq Documents.VALID
        ]
    ]
