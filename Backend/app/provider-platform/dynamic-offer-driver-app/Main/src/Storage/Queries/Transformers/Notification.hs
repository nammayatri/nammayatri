module Storage.Queries.Transformers.Notification where

import qualified Domain.Types.DriverFee as DF
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.Notification as Domain
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import Storage.Beam.Notification as BeamI
import qualified Storage.Queries.DriverFee as QDF

getMerchantOperatingCityId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> m (Kernel.Types.Id.Id DMOC.MerchantOperatingCity))
getMerchantOperatingCityId merchantOperatingCityId id driverFeeId = do
  merchantOperatingCityId' <- case merchantOperatingCityId of
    Just opcity -> return $ Id opcity
    Nothing -> do
      dfee <- QDF.findById (Id driverFeeId) >>= fromMaybeM (DriverFeeNotFound driverFeeId)
      updateMerchantOperatingCityIdByNotificationId (Id id) (DF.merchantOperatingCityId dfee)
      return $ dfee.merchantOperatingCityId
  pure merchantOperatingCityId'

updateMerchantOperatingCityIdByNotificationId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Domain.Notification -> Id DMOC.MerchantOperatingCity -> m ()
updateMerchantOperatingCityIdByNotificationId notificationId merchantOperatingCityId = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamI.merchantOperatingCityId (Just $ getId merchantOperatingCityId),
      Se.Set BeamI.updatedAt now
    ]
    [Se.Is BeamI.id (Se.Eq $ getId notificationId)]
