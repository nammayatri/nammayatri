{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.MerchantOnboarding where

import qualified Domain.Types.MerchantOnboarding
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.MerchantOnboarding as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MerchantOnboarding.MerchantOnboarding -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.MerchantOnboarding.MerchantOnboarding] -> m ())
createMany = traverse_ create

deleteById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.MerchantOnboarding.MerchantOnboarding -> m ())
deleteById id = do deleteWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findAllByOnboardingType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Domain.Types.MerchantOnboarding.OnboardingType -> m [Domain.Types.MerchantOnboarding.MerchantOnboarding])
findAllByOnboardingType limit offset onboardingType = do findAllWithOptionsKV [Se.Is Beam.onboardingType $ Se.Eq onboardingType] (Se.Desc Beam.createdAt) limit offset

findAllByRequestorId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m [Domain.Types.MerchantOnboarding.MerchantOnboarding])
findAllByRequestorId requestorId = do findAllWithKV [Se.Is Beam.requestorId $ Se.Eq requestorId]

findAllByStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MerchantOnboarding.OnboardingStatus -> m [Domain.Types.MerchantOnboarding.MerchantOnboarding])
findAllByStatus status = do findAllWithKV [Se.Is Beam.status $ Se.Eq status]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.MerchantOnboarding.MerchantOnboarding -> m (Maybe Domain.Types.MerchantOnboarding.MerchantOnboarding))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByRequestorIdAndOnboardingType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Domain.Types.MerchantOnboarding.OnboardingType -> m (Maybe Domain.Types.MerchantOnboarding.MerchantOnboarding))
findByRequestorIdAndOnboardingType requestorId onboardingType = do findOneWithKV [Se.And [Se.Is Beam.requestorId $ Se.Eq requestorId, Se.Is Beam.onboardingType $ Se.Eq onboardingType]]

updateOnboardingRemarks :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.MerchantOnboarding.MerchantOnboarding -> m ())
updateOnboardingRemarks remarks id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.remarks remarks, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateOnboardingStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.MerchantOnboarding.OnboardingStatus -> Kernel.Types.Id.Id Domain.Types.MerchantOnboarding.MerchantOnboarding -> m ())
updateOnboardingStatus status id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateOnboardingStatusAndRemarks ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.MerchantOnboarding.OnboardingStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.MerchantOnboarding.MerchantOnboarding -> m ())
updateOnboardingStatusAndRemarks status remarks id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.remarks remarks, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOnboarding.MerchantOnboarding -> m (Maybe Domain.Types.MerchantOnboarding.MerchantOnboarding))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MerchantOnboarding.MerchantOnboarding -> m ())
updateByPrimaryKey (Domain.Types.MerchantOnboarding.MerchantOnboarding {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.description description,
      Se.Set Beam.onboardingType onboardingType,
      Se.Set Beam.remarks remarks,
      Se.Set Beam.requestorId requestorId,
      Se.Set Beam.status status,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.MerchantOnboarding Domain.Types.MerchantOnboarding.MerchantOnboarding where
  fromTType' (Beam.MerchantOnboardingT {..}) = do
    pure $
      Just
        Domain.Types.MerchantOnboarding.MerchantOnboarding
          { createdAt = createdAt,
            description = description,
            id = Kernel.Types.Id.Id id,
            onboardingType = onboardingType,
            remarks = remarks,
            requestorId = requestorId,
            status = status,
            updatedAt = updatedAt
          }

instance ToTType' Beam.MerchantOnboarding Domain.Types.MerchantOnboarding.MerchantOnboarding where
  toTType' (Domain.Types.MerchantOnboarding.MerchantOnboarding {..}) = do
    Beam.MerchantOnboardingT
      { Beam.createdAt = createdAt,
        Beam.description = description,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.onboardingType = onboardingType,
        Beam.remarks = remarks,
        Beam.requestorId = requestorId,
        Beam.status = status,
        Beam.updatedAt = updatedAt
      }
