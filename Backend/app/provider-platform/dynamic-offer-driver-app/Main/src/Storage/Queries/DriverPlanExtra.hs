module Storage.Queries.DriverPlanExtra where

import qualified Data.Aeson as A
import Data.Time (UTCTime (..), addDays)
import Domain.Types.DriverInformation (DriverAutoPayStatus)
import Domain.Types.DriverPlan as Domain
import Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity as MOC
import Domain.Types.Person
import qualified Domain.Types.Plan as DPlan
import qualified Domain.Types.VehicleCategory as VC
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.DriverPlan as BeamDF
import Storage.Queries.OrphanInstances.DriverPlan ()

-- Extra code goes here --

findAllDriversToSendManualPaymentLinkWithLimit ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  DPlan.ServiceNames ->
  Id Merchant ->
  Id MOC.MerchantOperatingCity ->
  UTCTime ->
  Int ->
  m [DriverPlan]
findAllDriversToSendManualPaymentLinkWithLimit serviceName merchantId opCityId endTime limit = do
  findAllWithOptionsKV'
    [ Se.And
        [ Se.Is BeamDF.merchantId $ Se.Eq (Just merchantId.getId),
          Se.Is BeamDF.merchantOpCityId $ Se.Eq (Just opCityId.getId),
          Se.Is BeamDF.serviceName $ Se.Eq (Just serviceName),
          Se.Or [Se.Is BeamDF.lastPaymentLinkSentAtIstDate $ Se.LessThan (Just endTime), Se.Is BeamDF.lastPaymentLinkSentAtIstDate $ Se.Eq Nothing]
        ]
    ]
    (Just limit)
    Nothing

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

findByDriverIdAndServiceName :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> DPlan.ServiceNames -> m (Maybe DriverPlan)
findByDriverIdAndServiceName (Id driverId) serviceName =
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
  -- need DSL Fix
  findAllWithKV
    [ Se.And
        [ Se.Is BeamDF.driverId $ Se.In (getId <$> driverIds),
          Se.Is BeamDF.planType $ Se.Eq paymentMode,
          Se.Is BeamDF.enableServiceUsageCharge $ Se.Eq (Just True),
          Se.Is BeamDF.autoPayStatus $ Se.Eq autoPayStatus,
          Se.Is BeamDF.serviceName $ Se.Eq (Just serviceName)
        ]
    ]

findAllDriversEligibleForService ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  DPlan.ServiceNames ->
  Id Merchant ->
  Id MOC.MerchantOperatingCity ->
  UTCTime ->
  Int ->
  m [DriverPlan]
findAllDriversEligibleForService serviceName merchantId merchantOperatingCity endTime limit = do
  -- need DSL Fix
  findAllWithOptionsKV'
    [ Se.And
        [ Se.Is BeamDF.merchantId $ Se.Eq (Just merchantId.getId),
          Se.Is BeamDF.merchantOpCityId $ Se.Eq (Just merchantOperatingCity.getId),
          Se.Is BeamDF.serviceName $ Se.Eq (Just serviceName),
          Se.Is BeamDF.enableServiceUsageCharge $ Se.Eq (Just True),
          Se.Or [Se.Is BeamDF.lastBillGeneratedAt $ Se.LessThan (Just endTime), Se.Is BeamDF.lastBillGeneratedAt $ Se.Eq Nothing]
        ]
    ]
    (Just limit)
    Nothing

updatePlanIdByDriverIdAndServiceName :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> Id DPlan.Plan -> DPlan.ServiceNames -> Maybe VC.VehicleCategory -> Id MOC.MerchantOperatingCity -> m () -- ned DSL Fix
updatePlanIdByDriverIdAndServiceName (Id driverId) (Id planId) serviceName mbVehicleCategory merchantOperatingCity = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamDF.planId planId,
      Se.Set BeamDF.vehicleCategory mbVehicleCategory,
      Se.Set BeamDF.merchantOpCityId (Just merchantOperatingCity.getId),
      Se.Set BeamDF.updatedAt now
    ]
    [ Se.And
        [ Se.Is BeamDF.driverId (Se.Eq driverId),
          Se.Is BeamDF.serviceName $ Se.Eq (Just serviceName)
        ]
    ]

updateIsSubscriptionEnabledAtCategoryLevel ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id Person ->
  DPlan.ServiceNames ->
  Bool ->
  m ()
updateIsSubscriptionEnabledAtCategoryLevel driverId serviceName isSubscriptionEnabled = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamDF.isCategoryLevelSubscriptionEnabled (Just isSubscriptionEnabled),
      Se.Set BeamDF.updatedAt now
    ]
    [ Se.And
        [ Se.Is BeamDF.driverId $ Se.Eq (getId driverId),
          Se.Is BeamDF.serviceName $ Se.Eq (Just serviceName)
        ]
    ]

updateLastBillGeneratedAt ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id Person ->
  DPlan.ServiceNames ->
  UTCTime ->
  m ()
updateLastBillGeneratedAt driverId serviceName endTime = do
  updateOneWithKV
    [Se.Set BeamDF.lastBillGeneratedAt (Just endTime)]
    [ Se.And
        [ Se.Is BeamDF.driverId $ Se.Eq (getId driverId),
          Se.Is BeamDF.serviceName $ Se.Eq (Just serviceName)
        ]
    ]

addDaysToUTC :: Integer -> UTCTime -> UTCTime
addDaysToUTC n (UTCTime day diffTime) =
  UTCTime (addDays n day) diffTime

updateAllWithWaiveOffPercantageAndType ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  [WaiveOffEntity] ->
  m ()
updateAllWithWaiveOffPercantageAndType waiveOffEntityArray = do
  mapM_ (\we -> updateWaiveOffPercantageAndType we) waiveOffEntityArray

updateWaiveOffPercantageAndType ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  WaiveOffEntity ->
  m ()
updateWaiveOffPercantageAndType waiveOffEntity = do
  now <- getCurrentTime
  let validTill = addDaysToUTC waiveOffEntity.daysValidFor now
  updateOneWithKV
    [ Se.Set BeamDF.waiverOffPercentage $ Just waiveOffEntity.percentage,
      Se.Set BeamDF.waiveOfMode $ Just waiveOffEntity.waiveOfMode,
      Se.Set BeamDF.waiveOffEnabledOn $ Just now,
      Se.Set BeamDF.waiveOffValidTill $ Just validTill,
      Se.Set BeamDF.updatedAt now
    ]
    [ Se.And
        [ Se.Is BeamDF.driverId $ Se.Eq (waiveOffEntity.driverId),
          Se.Is BeamDF.serviceName $ Se.Eq (Just waiveOffEntity.serviceName)
        ]
    ]
