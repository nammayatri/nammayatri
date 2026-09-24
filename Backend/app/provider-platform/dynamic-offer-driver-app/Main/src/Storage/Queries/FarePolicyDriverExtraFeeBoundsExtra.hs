{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FarePolicyDriverExtraFeeBoundsExtra where

import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.FarePolicyDriverExtraFeeBounds as DTDEFB
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id as KTI
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FarePolicyDriverExtraFeeBounds as BeamDEFB
import Storage.Queries.OrphanInstances.FarePolicyDriverExtraFeeBounds

type FullDriverExtraFeeBounds = (KTI.Id DFP.FarePolicy, DFP.DriverExtraFeeBounds)

toDomainType :: FullDriverExtraFeeBounds -> DTDEFB.FarePolicyDriverExtraFeeBounds
toDomainType (KTI.Id farePolicyId, DFP.DriverExtraFeeBounds {..}) =
  DTDEFB.FarePolicyDriverExtraFeeBounds
    { id = Nothing,
      ..
    }

toFullType :: DTDEFB.FarePolicyDriverExtraFeeBounds -> FullDriverExtraFeeBounds
toFullType DTDEFB.FarePolicyDriverExtraFeeBounds {..} =
  (KTI.Id farePolicyId, DFP.DriverExtraFeeBounds {..})

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => FullDriverExtraFeeBounds -> m ()
create = createWithKV . toDomainType

findByFarePolicyIdAndStartDistance :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DFP.FarePolicy -> Meters -> m (Maybe FullDriverExtraFeeBounds)
findByFarePolicyIdAndStartDistance (Id farePolicyId) startDistance = do
  results <- findAllWithKV [Se.And [Se.Is BeamDEFB.farePolicyId $ Se.Eq farePolicyId, Se.Is BeamDEFB.startDistance $ Se.Eq startDistance]]
  pure $ toFullType <$> listToMaybe results

update :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DFP.FarePolicy -> Meters -> HighPrecMoney -> HighPrecMoney -> m ()
update (Id farePolicyId) startDistance minFee maxFee =
  updateWithKV
    [ Se.Set BeamDEFB.minFee $ roundToIntegral minFee,
      Se.Set BeamDEFB.maxFee $ roundToIntegral maxFee,
      Se.Set BeamDEFB.minFeeAmount $ Just minFee,
      Se.Set BeamDEFB.maxFeeAmount $ Just maxFee
    ]
    [Se.And [Se.Is BeamDEFB.farePolicyId $ Se.Eq farePolicyId, Se.Is BeamDEFB.startDistance $ Se.Eq startDistance]]

findAll' :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DFP.FarePolicy -> m [FullDriverExtraFeeBounds]
findAll' farePolicyId = do
  results <- findAllWithOptionsKV [Se.Is BeamDEFB.farePolicyId $ Se.Eq (getId farePolicyId)] (Se.Asc BeamDEFB.startDistance) Nothing Nothing
  pure $ toFullType <$> results

deleteAll' :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DFP.FarePolicy -> m ()
deleteAll' farePolicyId = deleteWithKV [Se.Is BeamDEFB.farePolicyId $ Se.Eq (getId farePolicyId)]
