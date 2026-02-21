module Storage.Queries.DriverGstinExtra where

import Domain.Types.DriverGstin
import qualified Domain.Types.Person as DP
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.DriverGstin as Beam
import Storage.Queries.OrphanInstances.DriverGstin ()

-- Extra code goes here --

findUnInvalidByGstNumber :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) => Text -> m (Maybe DriverGstin)
findUnInvalidByGstNumber gstNumber = do
  gstNumberHash <- getDbHash gstNumber
  listToMaybe
    <$> findAllWithKV
      [ Se.And
          [ Se.Is Beam.gstinHash $ Se.Eq gstNumberHash,
            Se.Is Beam.verificationStatus $ Se.Not $ Se.Eq Documents.INVALID
          ]
      ]

findAllByEncryptedGstNumber :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => DbHash -> m [DriverGstin]
findAllByEncryptedGstNumber gstNumberHash = do
  findAllWithKV
    [Se.Is Beam.gstinHash $ Se.Eq gstNumberHash]

findByGstNumberAndNotInValid :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> m (Maybe DriverGstin)
findByGstNumberAndNotInValid personId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.driverId $ Se.Eq personId.getId,
          Se.Is Beam.verificationStatus $ Se.In [Documents.VALID, Documents.PENDING]
        ]
    ]

upsertGstinRecord :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r, EncFlow m r) => DriverGstin -> m ()
upsertGstinRecord a@DriverGstin {..} =
  findOneWithKV [Se.Is Beam.driverId $ Se.Eq driverId.getId] >>= \case
    Just _ -> do
      now <- getCurrentTime
      updateOneWithKV
        [ Se.Set Beam.driverName driverName,
          Se.Set Beam.gstinEncrypted (gstin & unEncrypted . encrypted),
          Se.Set Beam.documentImageId1 documentImageId1.getId,
          Se.Set Beam.gstinHash (gstin & hash),
          Se.Set Beam.updatedAt now,
          Se.Set Beam.verificationStatus verificationStatus
        ]
        [Se.Is Beam.driverId $ Se.Eq driverId.getId]
    Nothing -> createWithKV a
