{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DriverPanCardExtra where

import Domain.Types.DriverPanCard
import qualified Domain.Types.IdfyVerification as Domain
import qualified Domain.Types.Person as DP
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverPanCard as Beam
import Storage.Queries.OrphanInstances.DriverPanCard

-- Extra code goes here --

findByPanNumber :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) => Text -> m (Maybe DriverPanCard)
findByPanNumber panNumber = do
  panNumberHash <- getDbHash panNumber
  findOneWithKV [Se.Is Beam.panCardNumberHash $ Se.Eq panNumberHash]

findByPanNumberAndNotInValid :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> m (Maybe DriverPanCard)
findByPanNumberAndNotInValid personId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq personId.getId,
          Se.Is Beam.verificationStatus $ Se.In [Domain.VALID, Domain.PENDING]
        ]
    ]
