{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.MerchantOnboardingStep where

import qualified Data.Aeson
import qualified Domain.Types.MerchantOnboardingStep
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.MerchantOnboardingStep as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MerchantOnboardingStep.MerchantOnboardingStep -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.MerchantOnboardingStep.MerchantOnboardingStep] -> m ())
createMany = traverse_ create

deleteByOnboardingId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m ())
deleteByOnboardingId merchantOnboardingId = do deleteWithKV [Se.Is Beam.merchantOnboardingId $ Se.Eq merchantOnboardingId]

findByMerchantOnboardingId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m [Domain.Types.MerchantOnboardingStep.MerchantOnboardingStep])
findByMerchantOnboardingId merchantOnboardingId = do findAllWithKV [Se.Is Beam.merchantOnboardingId $ Se.Eq merchantOnboardingId]

findByStepId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOnboardingStep.MerchantOnboardingStep -> m (Maybe Domain.Types.MerchantOnboardingStep.MerchantOnboardingStep))
findByStepId id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateDependency ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  ([Kernel.Types.Id.Id Domain.Types.MerchantOnboardingStep.MerchantOnboardingStep] -> Kernel.Types.Id.Id Domain.Types.MerchantOnboardingStep.MerchantOnboardingStep -> m ())
updateDependency dependency id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.dependency (Kernel.Types.Id.getId <$> dependency), Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateStepPayload :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Data.Aeson.Value -> Kernel.Types.Id.Id Domain.Types.MerchantOnboardingStep.MerchantOnboardingStep -> m ())
updateStepPayload payload id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.payload payload, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateStepRemarks :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.MerchantOnboardingStep.MerchantOnboardingStep -> m ())
updateStepRemarks remarks id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.remarks remarks, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateStepStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.MerchantOnboardingStep.StepStatus -> Kernel.Types.Id.Id Domain.Types.MerchantOnboardingStep.MerchantOnboardingStep -> m ())
updateStepStatus status id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOnboardingStep.MerchantOnboardingStep -> m (Maybe Domain.Types.MerchantOnboardingStep.MerchantOnboardingStep))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MerchantOnboardingStep.MerchantOnboardingStep -> m ())
updateByPrimaryKey (Domain.Types.MerchantOnboardingStep.MerchantOnboardingStep {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.dependency (Kernel.Types.Id.getId <$> dependency),
      Se.Set Beam.isAdminOnly isAdminOnly,
      Se.Set Beam.isApprovalRequired isApprovalRequired,
      Se.Set Beam.merchantOnboardingId merchantOnboardingId,
      Se.Set Beam.payload payload,
      Se.Set Beam.remarks remarks,
      Se.Set Beam.status status,
      Se.Set Beam.stepDescription stepDescription,
      Se.Set Beam.stepNameIdentifier stepNameIdentifier,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.MerchantOnboardingStep Domain.Types.MerchantOnboardingStep.MerchantOnboardingStep where
  fromTType' (Beam.MerchantOnboardingStepT {..}) = do
    pure $
      Just
        Domain.Types.MerchantOnboardingStep.MerchantOnboardingStep
          { createdAt = createdAt,
            dependency = Kernel.Types.Id.Id <$> dependency,
            id = Kernel.Types.Id.Id id,
            isAdminOnly = isAdminOnly,
            isApprovalRequired = isApprovalRequired,
            merchantOnboardingId = merchantOnboardingId,
            payload = payload,
            remarks = remarks,
            status = status,
            stepDescription = stepDescription,
            stepNameIdentifier = stepNameIdentifier,
            updatedAt = updatedAt
          }

instance ToTType' Beam.MerchantOnboardingStep Domain.Types.MerchantOnboardingStep.MerchantOnboardingStep where
  toTType' (Domain.Types.MerchantOnboardingStep.MerchantOnboardingStep {..}) = do
    Beam.MerchantOnboardingStepT
      { Beam.createdAt = createdAt,
        Beam.dependency = Kernel.Types.Id.getId <$> dependency,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isAdminOnly = isAdminOnly,
        Beam.isApprovalRequired = isApprovalRequired,
        Beam.merchantOnboardingId = merchantOnboardingId,
        Beam.payload = payload,
        Beam.remarks = remarks,
        Beam.status = status,
        Beam.stepDescription = stepDescription,
        Beam.stepNameIdentifier = stepNameIdentifier,
        Beam.updatedAt = updatedAt
      }
