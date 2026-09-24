{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FareParametersSlabDetailsExtra where

import qualified Domain.Types.FareParameters as Domain
import qualified Domain.Types.FareParametersSlabDetails as DTFPSD
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id as KTI
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FareParametersSlabDetails as BeamFPSD
import Storage.Queries.OrphanInstances.FareParametersSlabDetails

type FullFareParametersSlabDetails = (KTI.Id Domain.FareParameters, Domain.FParamsSlabDetails)

toFullType :: DTFPSD.FareParametersSlabDetails -> FullFareParametersSlabDetails
toFullType DTFPSD.FareParametersSlabDetails {..} =
  (KTI.Id fareParametersId, Domain.FParamsSlabDetails {..})

findById' :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => KTI.Id Domain.FareParameters -> m (Maybe FullFareParametersSlabDetails)
findById' (KTI.Id fareParametersId') = do
  result <- findOneWithKV [Se.Is BeamFPSD.fareParametersId $ Se.Eq fareParametersId']
  pure $ toFullType <$> result
