module Storage.Queries.Transformers.DriverFee where

import Domain.Types.DriverFee
import Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, Currency, EsqDBFlow, HighPrecMoney, MonadFlow, fromMaybeM)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverFee as BeamDF
import qualified Storage.Queries.Person as QP

getMerchantOperatingCityId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Maybe Kernel.Prelude.Text -> Kernel.Prelude.Text -> Text -> m (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity)
getMerchantOperatingCityId merchantOperatingCityId driverId id = do
  merchantOperatingCityId' <- case merchantOperatingCityId of
    Nothing -> do
      person <- QP.findById (Id driverId) >>= fromMaybeM (PersonNotFound driverId)
      let opCity = person.merchantOperatingCityId
      updateMerchantOperatingCityId (Id id) opCity
      return opCity
    Just mOpCityId -> return $ Id mOpCityId
  return merchantOperatingCityId'

updateMerchantOperatingCityId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DriverFee -> Id MerchantOperatingCity -> m ()
updateMerchantOperatingCityId driverFeeId merchantOperatingCityId = do
  updateOneWithKV
    [Se.Set BeamDF.merchantOperatingCityId (Just merchantOperatingCityId.getId)]
    [Se.Is BeamDF.id (Se.Eq driverFeeId.getId)]

mkPlatformFee :: HighPrecMoney -> HighPrecMoney -> HighPrecMoney -> Currency -> PlatformFee
mkPlatformFee fee cgst sgst currency = PlatformFee {..}
