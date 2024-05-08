{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DriverPanCardExtra where

import Domain.Types.DriverPanCard
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverPanCard as Beam
import Storage.Queries.OrphanInstances.DriverPanCard

-- Extra code goes here --

findByPanNumber :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) => Text -> m (Maybe DriverPanCard)
findByPanNumber panNumber = do
  panNumberHash <- getDbHash panNumber
  findOneWithKV [Se.Is Beam.panCardNumberHash $ Se.Eq panNumberHash]
