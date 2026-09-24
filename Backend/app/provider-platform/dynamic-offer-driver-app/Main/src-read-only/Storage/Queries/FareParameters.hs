{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FareParameters (module Storage.Queries.FareParameters, module ReExport) where

import qualified Domain.Types.FareParameters
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FareParameters as Beam
import Storage.Queries.FareParametersExtra as ReExport

findAllIn :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Kernel.Types.Id.Id Domain.Types.FareParameters.FareParameters] -> m ([Domain.Types.FareParameters.FareParameters]))
findAllIn id = do findAllWithKV [Se.Is Beam.id $ Se.In (Kernel.Types.Id.getId <$> id)]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FareParameters.FareParameters -> m (Maybe Domain.Types.FareParameters.FareParameters))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]
