{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.GoHomeConfig where

import qualified Domain.Types.GoHomeConfig
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.GoHomeConfig as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.GoHomeConfig.GoHomeConfig -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.GoHomeConfig.GoHomeConfig] -> m ())
createMany = traverse_ create

findByMerchantOpCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m (Maybe Domain.Types.GoHomeConfig.GoHomeConfig))
findByMerchantOpCityId merchantOperatingCityId = do findOneWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m (Maybe Domain.Types.GoHomeConfig.GoHomeConfig))
findByPrimaryKey merchantOperatingCityId = do findOneWithKV [Se.And [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.GoHomeConfig.GoHomeConfig -> m ())
updateByPrimaryKey (Domain.Types.GoHomeConfig.GoHomeConfig {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.activeTime activeTime,
      Se.Set Beam.addStartWaypointAt (Kernel.Types.Common.getMeters addStartWaypointAt),
      Se.Set Beam.cancellationCnt cancellationCnt,
      Se.Set Beam.destRadiusMeters destRadiusMeters,
      Se.Set Beam.distanceUnit (Kernel.Prelude.Just distanceUnit),
      Se.Set Beam.enableGoHome enableGoHome,
      Se.Set Beam.goHomeBatchDelay (Kernel.Types.Common.getSeconds goHomeBatchDelay),
      Se.Set Beam.goHomeFromLocationRadius goHomeFromLocationRadius,
      Se.Set Beam.goHomeWayPointRadius goHomeWayPointRadius,
      Se.Set Beam.ignoreWaypointsTill (Kernel.Types.Common.getMeters ignoreWaypointsTill),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.newLocAllowedRadius (Kernel.Types.Common.getMeters newLocAllowedRadius),
      Se.Set Beam.numDriversForDirCheck numDriversForDirCheck,
      Se.Set Beam.numHomeLocations numHomeLocations,
      Se.Set Beam.startCnt startCnt,
      Se.Set Beam.updateHomeLocationAfterSec updateHomeLocationAfterSec,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]]

instance FromTType' Beam.GoHomeConfig Domain.Types.GoHomeConfig.GoHomeConfig where
  fromTType' (Beam.GoHomeConfigT {..}) = do
    pure $
      Just
        Domain.Types.GoHomeConfig.GoHomeConfig
          { activeTime = activeTime,
            addStartWaypointAt = Kernel.Types.Common.Meters addStartWaypointAt,
            cancellationCnt = cancellationCnt,
            createdAt = createdAt,
            destRadiusMeters = destRadiusMeters,
            distanceUnit = Kernel.Prelude.fromMaybe Kernel.Types.Common.Meter distanceUnit,
            enableGoHome = enableGoHome,
            goHomeBatchDelay = Kernel.Types.Common.Seconds goHomeBatchDelay,
            goHomeFromLocationRadius = goHomeFromLocationRadius,
            goHomeWayPointRadius = goHomeWayPointRadius,
            ignoreWaypointsTill = Kernel.Types.Common.Meters ignoreWaypointsTill,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            newLocAllowedRadius = Kernel.Types.Common.Meters newLocAllowedRadius,
            numDriversForDirCheck = numDriversForDirCheck,
            numHomeLocations = numHomeLocations,
            startCnt = startCnt,
            updateHomeLocationAfterSec = updateHomeLocationAfterSec,
            updatedAt = updatedAt
          }

instance ToTType' Beam.GoHomeConfig Domain.Types.GoHomeConfig.GoHomeConfig where
  toTType' (Domain.Types.GoHomeConfig.GoHomeConfig {..}) = do
    Beam.GoHomeConfigT
      { Beam.activeTime = activeTime,
        Beam.addStartWaypointAt = Kernel.Types.Common.getMeters addStartWaypointAt,
        Beam.cancellationCnt = cancellationCnt,
        Beam.createdAt = createdAt,
        Beam.destRadiusMeters = destRadiusMeters,
        Beam.distanceUnit = Kernel.Prelude.Just distanceUnit,
        Beam.enableGoHome = enableGoHome,
        Beam.goHomeBatchDelay = Kernel.Types.Common.getSeconds goHomeBatchDelay,
        Beam.goHomeFromLocationRadius = goHomeFromLocationRadius,
        Beam.goHomeWayPointRadius = goHomeWayPointRadius,
        Beam.ignoreWaypointsTill = Kernel.Types.Common.getMeters ignoreWaypointsTill,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.newLocAllowedRadius = Kernel.Types.Common.getMeters newLocAllowedRadius,
        Beam.numDriversForDirCheck = numDriversForDirCheck,
        Beam.numHomeLocations = numHomeLocations,
        Beam.startCnt = startCnt,
        Beam.updateHomeLocationAfterSec = updateHomeLocationAfterSec,
        Beam.updatedAt = updatedAt
      }
