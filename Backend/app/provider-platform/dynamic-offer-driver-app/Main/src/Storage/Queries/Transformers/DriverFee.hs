module Storage.Queries.Transformers.DriverFee where

import Control.Applicative ((<|>))
import Domain.Types.DriverFee
import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Plan as Plan
import qualified Domain.Types.VehicleCategory as DSV
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, Currency, EsqDBFlow, HighPrecMoney, MonadFlow, fromMaybeM)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverFee as BeamDF
import qualified Storage.CachedQueries.Plan as CQP
import qualified Storage.CachedQueries.SubscriptionConfig as QSC
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

getCategoryFromPlanOrSubscriptionConfig :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Maybe DSV.VehicleCategory -> Maybe Text -> Maybe Plan.PaymentMode -> Maybe Kernel.Prelude.Text -> Maybe Plan.ServiceNames -> Text -> Text -> m DSV.VehicleCategory
getCategoryFromPlanOrSubscriptionConfig vehicleCategory planId planMode mbMerchantOpCityId mbServiceName id driverId = do
  case vehicleCategory of
    Nothing -> do
      let serviceName = fromMaybe Plan.YATRI_SUBSCRIPTION mbServiceName
      (plan, subscriptionConf) <- do
        case (planId, planMode) of
          (Just pId, Just pMode) -> do
            plan <- CQP.findByIdAndPaymentModeWithServiceName (Id pId) pMode serviceName
            return (plan, Nothing)
          _ -> do
            subscriptionConfig <- do
              merchantOpCityId <- getMerchantOperatingCityId mbMerchantOpCityId driverId id
              QSC.findSubscriptionConfigsByMerchantOpCityIdAndServiceName merchantOpCityId Nothing serviceName
            return (Nothing, subscriptionConfig)
      let vehilceCategory = (plan <&> (.vehicleCategory)) <|> (subscriptionConf <&> (.defaultCityVehicleCategory))
      updateOneWithKV
        [Se.Set BeamDF.vehicleCategory vehilceCategory]
        [Se.Is BeamDF.id $ Se.Eq id]
      return $ fromMaybe DSV.AUTO_CATEGORY vehilceCategory
    Just vc -> return $ vc
