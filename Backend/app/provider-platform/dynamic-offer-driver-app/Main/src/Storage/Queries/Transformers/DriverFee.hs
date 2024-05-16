{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Transformers.DriverFee where

import Domain.Types.DriverFee
import qualified Domain.Types.Merchant
import Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.Merchant.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import qualified Kernel.Types.Id
import Kernel.Utils.Common (Currency, HighPrecMoney, KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverFee as BeamDF
import qualified Storage.Queries.Person as QP

getMerchantOperatingCityId :: KvDbFlow m r => (Maybe Kernel.Prelude.Text -> Kernel.Prelude.Text -> Text -> m (Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity))
getMerchantOperatingCityId merchantOperatingCityId driverId id = do
  merchantOperatingCityId' <- case merchantOperatingCityId of
    Nothing -> do
      person <- QP.findById (Id driverId) >>= fromMaybeM (PersonNotFound driverId)
      let opCity = person.merchantOperatingCityId
      updateMerchantOperatingCityId (Id id) opCity
      return opCity
    Just mOpCityId -> return $ Id mOpCityId
  return merchantOperatingCityId'

updateMerchantOperatingCityId :: KvDbFlow m r => Id DriverFee -> Id MerchantOperatingCity -> m ()
updateMerchantOperatingCityId driverFeeId merchantOperatingCityId = do
  updateOneWithKV
    [Se.Set BeamDF.merchantOperatingCityId (Just merchantOperatingCityId.getId)]
    [Se.Is BeamDF.id (Se.Eq driverFeeId.getId)]

mkPlatformFee :: HighPrecMoney -> HighPrecMoney -> HighPrecMoney -> Currency -> PlatformFee
mkPlatformFee fee cgst sgst currency = PlatformFee {..}
