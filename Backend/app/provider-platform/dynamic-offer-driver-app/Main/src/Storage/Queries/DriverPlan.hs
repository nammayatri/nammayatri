{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.DriverPlan where

import qualified Data.Aeson as A
import Domain.Types.DriverInformation (DriverAutoPayStatus)
import Domain.Types.DriverPlan as Domain
import Domain.Types.Mandate
import Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity as MOC
import Domain.Types.Person
import qualified Domain.Types.Plan as DPlan
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.DriverPlan as BeamDF
import qualified Storage.Queries.Person as QP
import Tools.Error

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => DriverPlan -> m ()
create = createWithKV

findByDriverId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> m (Maybe DriverPlan)
findByDriverId (Id driverId) = findOneWithKV [Se.Is BeamDF.driverId $ Se.Eq driverId]

findByDriverIdWithServiceName :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> DPlan.ServiceNames -> m (Maybe DriverPlan)
findByDriverIdWithServiceName (Id driverId) serviceName = do
  findOneWithKV
    [ Se.And
        [ Se.Is BeamDF.driverId $ Se.Eq driverId,
          Se.Is BeamDF.serviceName $ Se.Eq (Just serviceName)
        ]
    ]

findAllByDriverIdsPaymentModeAndServiceName ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  [Id Person] ->
  DPlan.PaymentMode ->
  DPlan.ServiceNames ->
  Maybe DriverAutoPayStatus ->
  m [DriverPlan]
findAllByDriverIdsPaymentModeAndServiceName driverIds paymentMode serviceName autoPayStatus = do
  findAllWithKV
    [ Se.And
        [ Se.Is BeamDF.driverId $ Se.In (getId <$> driverIds),
          Se.Is BeamDF.planType $ Se.Eq paymentMode,
          Se.Is BeamDF.enableServiceUsageCharge $ Se.Eq (Just True),
          Se.Is BeamDF.autoPayStatus $ Se.Eq autoPayStatus,
          Se.Is BeamDF.serviceName $ Se.Eq (Just serviceName)
        ]
    ]

findByMandateIdAndServiceName ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id Mandate ->
  DPlan.ServiceNames ->
  m (Maybe DriverPlan)
findByMandateIdAndServiceName (Id mandateId) serviceName = do
  findOneWithKV
    [ Se.And
        [ Se.Is BeamDF.mandateId $ Se.Eq (Just mandateId),
          Se.Is BeamDF.serviceName $ Se.Eq (Just serviceName)
        ]
    ]

findAllDriversEligibleForService ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  DPlan.ServiceNames ->
  Id Merchant ->
  Id MOC.MerchantOperatingCity ->
  m [DriverPlan]
findAllDriversEligibleForService serviceName merchantId merchantOperatingCity = do
  findAllWithKV
    [ Se.And
        [ Se.Is BeamDF.merchantId $ Se.Eq (Just merchantId.getId),
          Se.Is BeamDF.merchantOpCityId $ Se.Eq (Just merchantOperatingCity.getId),
          Se.Is BeamDF.serviceName $ Se.Eq (Just serviceName),
          Se.Is BeamDF.enableServiceUsageCharge $ Se.Eq (Just True)
        ]
    ]

updateEnableServiceUsageChargeByDriverIdAndServiceName :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> Bool -> DPlan.ServiceNames -> m ()
updateEnableServiceUsageChargeByDriverIdAndServiceName (Id driverId) value serviceName = do
  now <- getCurrentTime
  updateOneWithKV
    [Se.Set BeamDF.enableServiceUsageCharge (Just value), Se.Set BeamDF.updatedAt now]
    [ Se.And
        [ Se.Is BeamDF.driverId (Se.Eq driverId),
          Se.Is BeamDF.serviceName $ Se.Eq (Just serviceName)
        ]
    ]

updatePlanIdByDriverIdAndServiceName :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> Id DPlan.Plan -> DPlan.ServiceNames -> m ()
updatePlanIdByDriverIdAndServiceName (Id driverId) (Id planId) serviceName = do
  now <- getCurrentTime
  updateOneWithKV
    [Se.Set BeamDF.planId planId, Se.Set BeamDF.updatedAt now]
    [ Se.And
        [ Se.Is BeamDF.driverId (Se.Eq driverId),
          Se.Is BeamDF.serviceName $ Se.Eq (Just serviceName)
        ]
    ]

updateMandateIdByDriverIdAndServiceName :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> Id Mandate -> DPlan.ServiceNames -> m ()
updateMandateIdByDriverIdAndServiceName driverId (Id mandateId) serviceName = do
  now <- getCurrentTime
  updateOneWithKV
    [Se.Set BeamDF.mandateId (Just mandateId), Se.Set BeamDF.updatedAt now]
    [ Se.And
        [ Se.Is BeamDF.driverId (Se.Eq (getId driverId)),
          Se.Is BeamDF.serviceName $ Se.Eq (Just serviceName)
        ]
    ]

updatePaymentModeByDriverIdAndServiceName :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> DPlan.PaymentMode -> DPlan.ServiceNames -> m ()
updatePaymentModeByDriverIdAndServiceName driverId paymentMode serviceName = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamDF.planType paymentMode,
      Se.Set BeamDF.updatedAt now
    ]
    [ Se.And
        [ Se.Is BeamDF.driverId (Se.Eq (getId driverId)),
          Se.Is BeamDF.serviceName $ Se.Eq (Just serviceName)
        ]
    ]

updateMandateSetupDateByDriverIdAndServiceName :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> DPlan.ServiceNames -> m ()
updateMandateSetupDateByDriverIdAndServiceName driverId serviceName = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamDF.mandateSetupDate (Just now),
      Se.Set BeamDF.updatedAt now
    ]
    [ Se.And
        [ Se.Is BeamDF.driverId (Se.Eq (getId driverId)),
          Se.Is BeamDF.serviceName $ Se.Eq (Just serviceName)
        ]
    ]

updateAutoPayStatusAndPayerVpaByDriverIdAndServiceName :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> DPlan.ServiceNames -> Maybe DriverAutoPayStatus -> Maybe Text -> m ()
updateAutoPayStatusAndPayerVpaByDriverIdAndServiceName driverId serviceName mbAutoPayStatus mbPayerVpa = do
  now <- getCurrentTime
  updateOneWithKV
    ( [ Se.Set BeamDF.autoPayStatus mbAutoPayStatus,
        Se.Set BeamDF.updatedAt now
      ]
        <> [Se.Set BeamDF.payerVpa mbPayerVpa | isJust mbPayerVpa]
    )
    [ Se.And
        [ Se.Is BeamDF.driverId (Se.Eq (getId driverId)),
          Se.Is BeamDF.serviceName $ Se.Eq (Just serviceName)
        ]
    ]

updatesubscriptionServiceRelatedDataInDriverPlan ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id Person ->
  Domain.SubscriptionServiceRelatedData ->
  DPlan.ServiceNames ->
  m ()
updatesubscriptionServiceRelatedDataInDriverPlan driverId subscriptionServiceRelatedData serviceName = do
  let commodityDataRaw = A.toJSON subscriptionServiceRelatedData :: A.Value
      parsedData = A.fromJSON commodityDataRaw :: A.Result CommodityData
  commodityData <- case parsedData of
    A.Success commodityData' -> return commodityData'
    A.Error _ -> throwError $ InternalError "Error while parsing commodityData"
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamDF.rentedVehicleNumber commodityData.rentedVehicleNumber,
      Se.Set BeamDF.updatedAt now
    ]
    [ Se.And
        [ Se.Is BeamDF.driverId (Se.Eq (getId driverId)),
          Se.Is BeamDF.serviceName $ Se.Eq (Just serviceName)
        ]
    ]

updateCoinToCashByDriverId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> HighPrecMoney -> m ()
updateCoinToCashByDriverId driverId amountToAdd = do
  now <- getCurrentTime
  mbDriverPlan <- findByDriverId driverId
  case mbDriverPlan of
    Just driverPlan -> do
      updateWithKV
        [ Se.Set BeamDF.coinCovertedToCashLeft $ driverPlan.coinCovertedToCashLeft + amountToAdd,
          Se.Set BeamDF.updatedAt now
        ]
        [Se.Is BeamDF.driverId (Se.Eq (getId driverId))]
    Nothing -> pure ()

updateTotalCoinToCashByDriverId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> HighPrecMoney -> m ()
updateTotalCoinToCashByDriverId driverId totalcoinToAdd = do
  now <- getCurrentTime
  mbDriverPlan <- findByDriverId driverId
  case mbDriverPlan of
    Just driverPlan -> do
      updateWithKV
        [ Se.Set BeamDF.totalCoinsConvertedCash $ driverPlan.totalCoinsConvertedCash + totalcoinToAdd,
          Se.Set BeamDF.updatedAt now
        ]
        [Se.Is BeamDF.driverId (Se.Eq (getId driverId))]
    Nothing -> pure ()

updateCoinFieldsByDriverId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> HighPrecMoney -> m ()
updateCoinFieldsByDriverId driverId amount = do
  now <- getCurrentTime
  mbDriverPlan <- findByDriverId driverId
  case mbDriverPlan of
    Just driverPlan -> do
      updateWithKV
        [ Se.Set BeamDF.coinCovertedToCashLeft $ driverPlan.coinCovertedToCashLeft + amount,
          Se.Set BeamDF.totalCoinsConvertedCash $ driverPlan.totalCoinsConvertedCash + amount,
          Se.Set BeamDF.updatedAt now
        ]
        [Se.Is BeamDF.driverId (Se.Eq (getId driverId))]
    Nothing -> pure ()

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

instance FromTType' BeamDF.DriverPlan DriverPlan where
  fromTType' BeamDF.DriverPlanT {..} = do
    subscriptionServiceRelatedData <- do
      let isCommodityDataPresent = verifyCommodityData [rentedVehicleNumber]
          commodityData = if isCommodityDataPresent then Just $ CommodityData {rentedVehicleNumber} else Nothing
      let commodityDataRaw' = fmap (\cd -> A.toJSON cd :: A.Value) commodityData
      case commodityDataRaw' of
        Just cd -> do
          let parsedData = A.fromJSON cd :: A.Result SubscriptionServiceRelatedData
          case parsedData of
            A.Success subscriptionServiceRelatedData' -> return subscriptionServiceRelatedData'
            A.Error _ -> throwError $ InternalError "Error while parsing subscriptionServiceRelatedData"
        Nothing -> pure NoData
    case (merchantId, merchantOpCityId) of
      (Just merchantId', Just merchantOperatingCityId') -> do
        pure $ Just $ mkDriverPlan (Id merchantId') (Id merchantOperatingCityId') subscriptionServiceRelatedData
      _ -> do
        person <- QP.findById (Id driverId) >>= fromMaybeM (PersonNotFound driverId)
        updateMerchantIdAndOpCityByDriverAndServiceName (Id driverId) (person.merchantId) (person.merchantOperatingCityId) (fromMaybe DPlan.YATRI_SUBSCRIPTION serviceName)
        pure $ Just $ mkDriverPlan (person.merchantId) (person.merchantOperatingCityId) subscriptionServiceRelatedData
    where
      mkDriverPlan merchantId'' merchantOpCityId'' subscriptionServiceRelatedData' = do
        Domain.DriverPlan
          { driverId = Id driverId,
            planId = Id planId,
            planType = planType,
            mandateId = Id <$> mandateId,
            mandateSetupDate = mandateSetupDate,
            createdAt = createdAt,
            updatedAt = updatedAt,
            coinCovertedToCashLeft = coinCovertedToCashLeft,
            totalCoinsConvertedCash = totalCoinsConvertedCash,
            serviceName = fromMaybe DPlan.YATRI_SUBSCRIPTION serviceName,
            enableServiceUsageCharge = fromMaybe False enableServiceUsageCharge,
            autoPayStatus = autoPayStatus,
            merchantId = merchantId'',
            merchantOpCityId = merchantOpCityId'',
            payerVpa = payerVpa,
            subscriptionServiceRelatedData = subscriptionServiceRelatedData'
          }
      verifyCommodityData dataPoints = do
        let data' = catMaybes dataPoints
        not (null data')

instance ToTType' BeamDF.DriverPlan DriverPlan where
  toTType' DriverPlan {..} = do
    let commodityDataRaw' = A.toJSON subscriptionServiceRelatedData :: A.Value
        parsedData = A.fromJSON commodityDataRaw' :: A.Result CommodityData
    let commodityData = case parsedData of
          A.Success commodityData' -> Just commodityData'
          A.Error _ -> Nothing
    BeamDF.DriverPlanT
      { BeamDF.driverId = getId driverId,
        BeamDF.planId = getId planId,
        BeamDF.planType = planType,
        BeamDF.mandateId = getId <$> mandateId,
        BeamDF.mandateSetupDate = mandateSetupDate,
        BeamDF.createdAt = createdAt,
        BeamDF.updatedAt = updatedAt,
        BeamDF.coinCovertedToCashLeft = coinCovertedToCashLeft,
        BeamDF.totalCoinsConvertedCash = totalCoinsConvertedCash,
        BeamDF.serviceName = Just serviceName,
        BeamDF.enableServiceUsageCharge = Just enableServiceUsageCharge,
        BeamDF.autoPayStatus = autoPayStatus,
        BeamDF.merchantId = Just merchantId.getId,
        BeamDF.payerVpa = payerVpa,
        BeamDF.merchantOpCityId = Just merchantOpCityId.getId,
        BeamDF.rentedVehicleNumber = commodityData >>= (.rentedVehicleNumber)
      }
