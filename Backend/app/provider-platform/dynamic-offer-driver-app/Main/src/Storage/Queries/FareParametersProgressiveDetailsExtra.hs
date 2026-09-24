{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FareParametersProgressiveDetailsExtra where

import qualified Domain.Types.FareParameters as Domain
import qualified Domain.Types.FareParametersProgressiveDetails as DTFPPD
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id as KTI
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FareParametersProgressiveDetails as BeamFPPD
import Storage.Queries.OrphanInstances.FareParametersProgressiveDetails

type FullFareParametersProgressiveDetails = (KTI.Id Domain.FareParameters, Domain.FParamsProgressiveDetails)

toFullType :: DTFPPD.FareParametersProgressiveDetails -> FullFareParametersProgressiveDetails
toFullType DTFPPD.FareParametersProgressiveDetails {..} =
  (KTI.Id fareParametersId, Domain.FParamsProgressiveDetails {..})

findById' :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => KTI.Id Domain.FareParameters -> m (Maybe FullFareParametersProgressiveDetails)
findById' (KTI.Id fareParametersId') = do
  result <- findOneWithKV [Se.Is BeamFPPD.fareParametersId $ Se.Eq fareParametersId']
  pure $ toFullType <$> result

findDeadKmFareEarnings :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [KTI.Id Domain.FareParameters] -> m HighPrecMoney
findDeadKmFareEarnings fareParamIds = do
  results <- findAllWithKV [Se.Is BeamFPPD.fareParametersId $ Se.In $ KTI.getId <$> fareParamIds]
  pure $ sum $ (Domain.deadKmFare :: Domain.FParamsProgressiveDetails -> HighPrecMoney) . snd . toFullType <$> results
