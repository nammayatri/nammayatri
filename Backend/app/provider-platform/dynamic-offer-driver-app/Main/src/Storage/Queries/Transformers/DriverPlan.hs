{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Transformers.DriverPlan where

import qualified Data.Aeson as A
import Domain.Types.DriverInformation (DriverAutoPayStatus)
import Domain.Types.DriverPlan as Domain
import qualified Domain.Types.Extra.DriverPlan
import Domain.Types.Mandate
import Domain.Types.Merchant
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.MerchantOperatingCity as MOC
import Domain.Types.Person
import qualified Domain.Types.Plan
import qualified Domain.Types.Plan as DPlan
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverPlan as BeamDF
import qualified Storage.Queries.Person as QP

getMerchantId :: (MonadFlow m, EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.Plan.ServiceNames -> m (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant))
getMerchantId merchantId driverId serviceName = do
  case merchantId of
    Just merchantId' -> do
      pure $ (Id merchantId')
    _ -> do
      person <- QP.findById (Id driverId) >>= fromMaybeM (PersonNotFound driverId)
      updateMerchantIdAndOpCityByDriverAndServiceName (Id driverId) (person.merchantId) (person.merchantOperatingCityId) (fromMaybe DPlan.YATRI_SUBSCRIPTION serviceName)
      pure $ person.merchantId

getMerchantOpCityId :: (MonadFlow m, EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.Plan.ServiceNames -> m (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity))
getMerchantOpCityId merchantOpCityId driverId serviceName = do
  case merchantOpCityId of
    Just merchantOperatingCityId' -> do
      pure $ (Id merchantOperatingCityId')
    _ -> do
      person <- QP.findById (Id driverId) >>= fromMaybeM (PersonNotFound driverId)
      updateMerchantIdAndOpCityByDriverAndServiceName (Id driverId) (person.merchantId) (person.merchantOperatingCityId) (fromMaybe DPlan.YATRI_SUBSCRIPTION serviceName)
      pure $ (person.merchantOperatingCityId)

getSubscriptionServiceRelatedData :: MonadFlow m => Kernel.Prelude.Maybe Kernel.Prelude.Text -> m (Domain.Types.Extra.DriverPlan.SubscriptionServiceRelatedData)
getSubscriptionServiceRelatedData rentedVehicleNumber = do
  subscriptionServiceRelatedData <- do
    let isCommodityDataPresent = verifyCommodityData [rentedVehicleNumber]
        commodityData = if isCommodityDataPresent then Just $ CommodityData {rentedVehicleNumber} else Nothing
    let commodityDataRaw' = fmap (\cd -> A.toJSON cd :: A.Value) commodityData
    case commodityDataRaw' of
      Just cd -> do
        let parsedData = A.fromJSON cd :: A.Result Domain.Types.Extra.DriverPlan.SubscriptionServiceRelatedData
        case parsedData of
          A.Success subscriptionServiceRelatedData' -> return subscriptionServiceRelatedData'
          A.Error _ -> throwError $ InternalError "Error while parsing subscriptionServiceRelatedData"
      Nothing -> pure NoData
  pure subscriptionServiceRelatedData
  where
    verifyCommodityData dataPoints = do
      let data' = catMaybes dataPoints
      not (null data')

getCommodityData :: Domain.Types.Extra.DriverPlan.SubscriptionServiceRelatedData -> Maybe Text
getCommodityData subscriptionServiceRelatedData = do
  let commodityDataRaw' = A.toJSON subscriptionServiceRelatedData :: A.Value
      parsedData = A.fromJSON commodityDataRaw' :: A.Result CommodityData
  let commodityData = case parsedData of
        A.Success commodityData' -> Just commodityData'
        A.Error _ -> Nothing
  commodityData >>= (.rentedVehicleNumber)

updateMerchantIdAndOpCityByDriverAndServiceName ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id Person ->
  Id Merchant ->
  Id MOC.MerchantOperatingCity ->
  DPlan.ServiceNames ->
  m ()
updateMerchantIdAndOpCityByDriverAndServiceName driverId merchantId merchantOpCityId serviceName = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamDF.merchantId (Just merchantId.getId),
      Se.Set BeamDF.merchantOpCityId (Just merchantOpCityId.getId),
      Se.Set BeamDF.updatedAt now
    ]
    [ Se.And
        [ Se.Is BeamDF.driverId (Se.Eq (getId driverId)),
          Se.Is BeamDF.serviceName $ Se.Eq (Just serviceName)
        ]
    ]
