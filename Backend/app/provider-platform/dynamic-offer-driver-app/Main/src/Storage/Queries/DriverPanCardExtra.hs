module Storage.Queries.DriverPanCardExtra where

import Control.Lens ((^?), _head)
import Domain.Types.DriverPanCard
import qualified Domain.Types.Person as DP
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverPanCard as Beam
import Storage.Queries.OrphanInstances.DriverPanCard ()

-- Extra code goes here --

findUnInvalidByPanNumber :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) => Text -> m (Maybe DriverPanCard)
findUnInvalidByPanNumber panNumber = do
  panNumberHash <- getDbHash panNumber
  (^? _head)
    <$> findAllWithKV
      [ Se.And
          [ Se.Is Beam.panCardNumberHash $ Se.Eq panNumberHash,
            Se.Is Beam.verificationStatus $ Se.Not $ Se.Eq Documents.INVALID
          ]
      ]

findAllByEncryptedPanNumber :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => DbHash -> m [DriverPanCard]
findAllByEncryptedPanNumber panNumberHash = do
  findAllWithKV
    [Se.Is Beam.panCardNumberHash $ Se.Eq panNumberHash]

findByPanNumberAndNotInValid :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> m (Maybe DriverPanCard)
findByPanNumberAndNotInValid personId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq personId.getId,
          Se.Is Beam.verificationStatus $ Se.In [Documents.VALID, Documents.PENDING]
        ]
    ]

upsertPanRecord :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r, EncFlow m r) => DriverPanCard -> m ()
upsertPanRecord a@DriverPanCard {..} =
  findOneWithKV [Se.Is Beam.driverId $ Se.Eq driverId.getId] >>= \case
    Just _ ->
      updateOneWithKV
        [ Se.Set Beam.consentTimestamp consentTimestamp,
          Se.Set Beam.driverDob driverDob,
          Se.Set Beam.driverName driverName,
          Se.Set Beam.documentImageId1 documentImageId1.getId,
          Se.Set Beam.panCardNumberHash (panCardNumber & hash),
          Se.Set Beam.updatedAt updatedAt,
          Se.Set Beam.verificationStatus verificationStatus,
          Se.Set Beam.driverNameOnGovtDB driverNameOnGovtDB
        ]
        [Se.Is Beam.driverId $ Se.Eq driverId.getId]
    Nothing -> createWithKV a
