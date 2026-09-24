{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FareParametersRentalDetailsExtra where

import qualified Domain.Types.FareParameters as Domain
import qualified Domain.Types.FareParametersRentalDetails as DTFPRD
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id as KTI
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FareParametersRentalDetails as BeamFPRD
import Storage.Queries.OrphanInstances.FareParametersRentalDetails

type FullFareParametersRentalDetails = (KTI.Id Domain.FareParameters, Domain.FParamsRentalDetails)

toFullType :: DTFPRD.FareParametersRentalDetails -> FullFareParametersRentalDetails
toFullType DTFPRD.FareParametersRentalDetails {..} =
  (KTI.Id fareParametersId, Domain.FParamsRentalDetails {..})

findById' :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => KTI.Id Domain.FareParameters -> m (Maybe FullFareParametersRentalDetails)
findById' (KTI.Id fareParametersId') = do
  result <- findOneWithKV [Se.Is BeamFPRD.fareParametersId $ Se.Eq fareParametersId']
  pure $ toFullType <$> result

findDeadKmFareEarnings :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [KTI.Id Domain.FareParameters] -> m HighPrecMoney
findDeadKmFareEarnings fareParamIds = do
  results <- findAllWithKV [Se.Is BeamFPRD.fareParametersId $ Se.In $ KTI.getId <$> fareParamIds]
  pure $ sum $ (Domain.deadKmFare :: Domain.FParamsRentalDetails -> HighPrecMoney) . snd . toFullType <$> results
