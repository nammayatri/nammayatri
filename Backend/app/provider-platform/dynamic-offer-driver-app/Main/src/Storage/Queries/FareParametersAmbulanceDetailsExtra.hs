{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FareParametersAmbulanceDetailsExtra where

import qualified Domain.Types.FareParameters as Domain
import qualified Domain.Types.FareParametersAmbulanceDetails as DTFPAD
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id as KTI
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FareParametersAmbulanceDetails as BeamFPAD
import Storage.Queries.OrphanInstances.FareParametersAmbulanceDetails

type FullFareParametersAmbulanceDetails = (KTI.Id Domain.FareParameters, Domain.FParamsAmbulanceDetails)

toFullType :: DTFPAD.FareParametersAmbulanceDetails -> FullFareParametersAmbulanceDetails
toFullType DTFPAD.FareParametersAmbulanceDetails {..} =
  (KTI.Id fareParametersId, Domain.FParamsAmbulanceDetails {..})

findById' :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => KTI.Id Domain.FareParameters -> m (Maybe FullFareParametersAmbulanceDetails)
findById' (KTI.Id fareParametersId') = do
  result <- findOneWithKV [Se.Is BeamFPAD.fareParametersId $ Se.Eq fareParametersId']
  pure $ toFullType <$> result

update :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => KTI.Id Domain.FareParameters -> Domain.FParamsAmbulanceDetails -> m ()
update id Domain.FParamsAmbulanceDetails {..} =
  updateOneWithKV
    [Se.Set BeamFPAD.distBasedFare distBasedFare]
    [Se.Is BeamFPAD.fareParametersId (Se.Eq id.getId)]
