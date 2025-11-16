{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DriverPlan (module Storage.Queries.DriverPlan, module ReExport) where

import qualified Domain.Types.DriverPlan
import qualified Domain.Types.Extra.Plan
import qualified Domain.Types.Mandate
import qualified Domain.Types.Person
import qualified Domain.Types.Plan
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverPlan as Beam
import Storage.Queries.DriverPlanExtra as ReExport
import qualified Storage.Queries.Transformers.DriverPlan

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverPlan.DriverPlan -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.DriverPlan.DriverPlan] -> m ())
createMany = traverse_ create

findByDriverIdWithServiceName ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Types.Extra.Plan.ServiceNames -> m (Maybe Domain.Types.DriverPlan.DriverPlan))
findByDriverIdWithServiceName driverId serviceName = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId),
          Se.Is Beam.serviceName $ Se.Eq (Kernel.Prelude.Just serviceName)
        ]
    ]

findByMandateIdAndServiceName ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Mandate.Mandate) -> Domain.Types.Extra.Plan.ServiceNames -> m (Maybe Domain.Types.DriverPlan.DriverPlan))
findByMandateIdAndServiceName mandateId serviceName = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.mandateId $ Se.Eq (Kernel.Types.Id.getId <$> mandateId),
          Se.Is Beam.serviceName $ Se.Eq (Kernel.Prelude.Just serviceName)
        ]
    ]

updateEnableServiceUsageChargeByDriverIdAndServiceName ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Types.Extra.Plan.ServiceNames -> m ())
updateEnableServiceUsageChargeByDriverIdAndServiceName enableServiceUsageCharge driverId serviceName = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.enableServiceUsageCharge (Kernel.Prelude.Just enableServiceUsageCharge),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId), Se.Is Beam.serviceName $ Se.Eq (Kernel.Prelude.Just serviceName)]]

updateFreeTrialByDriverIdAndServiceName ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Types.Extra.Plan.ServiceNames -> m ())
updateFreeTrialByDriverIdAndServiceName isOnFreeTrial driverId serviceName = do
  _now <- getCurrentTime
  updateOneWithKV
    [Se.Set Beam.isOnFreeTrial (Kernel.Prelude.Just isOnFreeTrial), Se.Set Beam.updatedAt _now]
    [ Se.And
        [ Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId),
          Se.Is Beam.serviceName $ Se.Eq (Kernel.Prelude.Just serviceName)
        ]
    ]

updateLastPaymentLinkSentAtDateByDriverIdAndServiceName ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Types.Extra.Plan.ServiceNames -> m ())
updateLastPaymentLinkSentAtDateByDriverIdAndServiceName lastPaymentLinkSentAtIstDate driverId serviceName = do
  _now <- getCurrentTime
  updateOneWithKV
    [Se.Set Beam.lastPaymentLinkSentAtIstDate lastPaymentLinkSentAtIstDate, Se.Set Beam.updatedAt _now]
    [ Se.And
        [ Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId),
          Se.Is Beam.serviceName $ Se.Eq (Kernel.Prelude.Just serviceName)
        ]
    ]

updateMandateIdByDriverIdAndServiceName ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Mandate.Mandate) -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Types.Extra.Plan.ServiceNames -> m ())
updateMandateIdByDriverIdAndServiceName mandateId driverId serviceName = do
  _now <- getCurrentTime
  updateOneWithKV
    [Se.Set Beam.mandateId (Kernel.Types.Id.getId <$> mandateId), Se.Set Beam.updatedAt _now]
    [ Se.And
        [ Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId),
          Se.Is Beam.serviceName $ Se.Eq (Kernel.Prelude.Just serviceName)
        ]
    ]

updateMandateSetupDateByDriverIdAndServiceName ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Types.Extra.Plan.ServiceNames -> m ())
updateMandateSetupDateByDriverIdAndServiceName mandateSetupDate driverId serviceName = do
  _now <- getCurrentTime
  updateOneWithKV
    [Se.Set Beam.mandateSetupDate mandateSetupDate, Se.Set Beam.updatedAt _now]
    [ Se.And
        [ Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId),
          Se.Is Beam.serviceName $ Se.Eq (Kernel.Prelude.Just serviceName)
        ]
    ]

updatePaymentModeByDriverIdAndServiceName ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.Plan.PaymentMode -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Types.Extra.Plan.ServiceNames -> m ())
updatePaymentModeByDriverIdAndServiceName planType driverId serviceName = do
  _now <- getCurrentTime
  updateOneWithKV
    [Se.Set Beam.planType planType, Se.Set Beam.updatedAt _now]
    [ Se.And
        [ Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId),
          Se.Is Beam.serviceName $ Se.Eq (Kernel.Prelude.Just serviceName)
        ]
    ]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.DriverPlan.DriverPlan))
findByPrimaryKey driverId = do findOneWithKV [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverPlan.DriverPlan -> m ())
updateByPrimaryKey (Domain.Types.DriverPlan.DriverPlan {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.autoPayStatus autoPayStatus,
      Se.Set Beam.coinCovertedToCashLeft coinCovertedToCashLeft,
      Se.Set Beam.enableServiceUsageCharge (Kernel.Prelude.Just enableServiceUsageCharge),
      Se.Set Beam.isCategoryLevelSubscriptionEnabled isCategoryLevelSubscriptionEnabled,
      Se.Set Beam.isOnFreeTrial (Kernel.Prelude.Just isOnFreeTrial),
      Se.Set Beam.lastBillGeneratedAt lastBillGeneratedAt,
      Se.Set Beam.lastPaymentLinkSentAtIstDate lastPaymentLinkSentAtIstDate,
      Se.Set Beam.mandateId (Kernel.Types.Id.getId <$> mandateId),
      Se.Set Beam.mandateSetupDate mandateSetupDate,
      Se.Set Beam.merchantId (Kernel.Prelude.Just (Kernel.Types.Id.getId merchantId)),
      Se.Set Beam.merchantOpCityId (Kernel.Prelude.Just (Kernel.Types.Id.getId merchantOpCityId)),
      Se.Set Beam.payerVpa payerVpa,
      Se.Set Beam.planId (Kernel.Types.Id.getId planId),
      Se.Set Beam.planType planType,
      Se.Set Beam.serviceName (Kernel.Prelude.Just serviceName),
      Se.Set Beam.rentedVehicleNumber (Storage.Queries.Transformers.DriverPlan.getCommodityData subscriptionServiceRelatedData),
      Se.Set Beam.totalAmountChargedForService (Kernel.Prelude.Just totalAmountChargedForService),
      Se.Set Beam.totalCoinsConvertedCash totalCoinsConvertedCash,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.vehicleCategory vehicleCategory,
      Se.Set Beam.waiveOfMode (Kernel.Prelude.Just waiveOfMode),
      Se.Set Beam.waiveOffEnabledOn waiveOffEnabledOn,
      Se.Set Beam.waiveOffValidTill waiveOffValidTill,
      Se.Set Beam.waiverOffPercentage (Kernel.Prelude.Just waiverOffPercentage)
    ]
    [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]]
