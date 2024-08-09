{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.NammaTagExtra where

import qualified Domain.Types.NammaTag
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Yudhishthira.Types
import Sequelize as Se
import qualified Storage.Beam.NammaTag as Beam
import Storage.Queries.OrphanInstances.NammaTag

-- Extra code goes here --
findAllByApplicationEvent :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Lib.Yudhishthira.Types.ApplicationEvent -> m [Domain.Types.NammaTag.NammaTag]
findAllByApplicationEvent event = do findAllWithKV [Se.And [Se.Is Beam.event $ Se.Eq (Just event)]]

findAllByChakra :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Lib.Yudhishthira.Types.Chakra -> m [Domain.Types.NammaTag.NammaTag]
findAllByChakra chakra = do findAllWithKV [Se.And [Se.Is Beam.chakra $ Se.Eq (Just chakra)]]
