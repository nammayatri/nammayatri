{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FarePolicyRentalDetailsDistanceBuffersExtra where

import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.FarePolicyRentalDetailsDistanceBuffers as DTFPRDB
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id as KTI
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FarePolicyRentalDetailsDistanceBuffers as BeamFPRDDB
import Storage.Queries.OrphanInstances.FarePolicyRentalDetailsDistanceBuffers

type FullFarePolicyRentalDetailsDistanceBuffers = (KTI.Id DFP.FarePolicy, DFP.FPRentalDetailsDistanceBuffers)

toDomainType :: FullFarePolicyRentalDetailsDistanceBuffers -> DTFPRDB.FarePolicyRentalDetailsDistanceBuffers
toDomainType (KTI.Id farePolicyId, DFP.FPRentalDetailsDistanceBuffers {..}) =
  DTFPRDB.FarePolicyRentalDetailsDistanceBuffers {..}

toFullType :: DTFPRDB.FarePolicyRentalDetailsDistanceBuffers -> FullFarePolicyRentalDetailsDistanceBuffers
toFullType DTFPRDB.FarePolicyRentalDetailsDistanceBuffers {..} =
  (KTI.Id farePolicyId, DFP.FPRentalDetailsDistanceBuffers {..})

findAll' :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DFP.FarePolicy -> m [FullFarePolicyRentalDetailsDistanceBuffers]
findAll' farePolicyId = do
  results <- findAllWithOptionsKV [Se.Is BeamFPRDDB.farePolicyId $ Se.Eq (getId farePolicyId)] (Se.Asc BeamFPRDDB.rideDuration) Nothing Nothing
  pure $ toFullType <$> results

delete :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DFP.FarePolicy -> m ()
delete farePolicyId = deleteWithKV [Se.Is BeamFPRDDB.farePolicyId $ Se.Eq (getId farePolicyId)]
