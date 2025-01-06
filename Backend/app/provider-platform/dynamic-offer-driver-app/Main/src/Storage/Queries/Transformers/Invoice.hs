module Storage.Queries.Transformers.Invoice where

import qualified Domain.Types.Invoice as Domain
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Invoice as BeamI
import qualified Storage.Queries.DriverFee as QDF

getMerchantOperatingCityId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> m (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity))
getMerchantOperatingCityId merchantOperatingCityId driverFeeId id = do
  merchantOperatingCityId' <- case merchantOperatingCityId of
    Just opCity -> return $ Id opCity
    Nothing -> do
      dfee <- QDF.findById (Id driverFeeId) >>= fromMaybeM (DriverFeeNotFound driverFeeId)
      updateMerchantOperatingCityIdByInvoiceId (Id id) (dfee.merchantOperatingCityId)
      return dfee.merchantOperatingCityId
  return merchantOperatingCityId'

updateMerchantOperatingCityIdByInvoiceId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Domain.Invoice -> Id DMOC.MerchantOperatingCity -> m ()
updateMerchantOperatingCityIdByInvoiceId invoiceId merchantOperatingCityId = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamI.merchantOperatingCityId (Just $ getId merchantOperatingCityId),
      Se.Set BeamI.updatedAt now
    ]
    [Se.Is BeamI.id (Se.Eq $ getId invoiceId)]
