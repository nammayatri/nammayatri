{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FareParametersInterCityDetailsExtra where

import qualified Domain.Types.FareParameters as Domain
import qualified Domain.Types.FareParametersInterCityDetails as DTFPICD
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id as KTI
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FareParametersInterCityDetails as BeamFPICD
import Storage.Queries.OrphanInstances.FareParametersInterCityDetails

type FullFareParametersInterCityDetails = (KTI.Id Domain.FareParameters, Domain.FParamsInterCityDetails)

toFullType :: DTFPICD.FareParametersInterCityDetails -> FullFareParametersInterCityDetails
toFullType DTFPICD.FareParametersInterCityDetails {..} =
  (KTI.Id fareParametersId, Domain.FParamsInterCityDetails {..})

findById' :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => KTI.Id Domain.FareParameters -> m (Maybe FullFareParametersInterCityDetails)
findById' (KTI.Id fareParametersId') = do
  result <- findOneWithKV [Se.Is BeamFPICD.fareParametersId $ Se.Eq fareParametersId']
  pure $ toFullType <$> result
