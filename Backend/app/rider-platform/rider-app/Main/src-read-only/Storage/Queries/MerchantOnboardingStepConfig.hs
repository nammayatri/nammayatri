{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.MerchantOnboardingStepConfig where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.MerchantOnboardingStepConfig
import qualified Storage.Beam.MerchantOnboardingStepConfig as Beam
import qualified Kernel.Prelude
import qualified Domain.Types.MerchantOnboarding
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MerchantOnboardingStepConfig.MerchantOnboardingStepConfig -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.MerchantOnboardingStepConfig.MerchantOnboardingStepConfig] -> m ())
createMany = traverse_ create
findByOnboardingType :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MerchantOnboarding.OnboardingType -> m ([Domain.Types.MerchantOnboardingStepConfig.MerchantOnboardingStepConfig]))
findByOnboardingType onboardingType = do findAllWithKV [Se.Is Beam.onboardingType $ Se.Eq onboardingType]
findByOnboardingTypeAndStepNameIdentifier :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                                             (Domain.Types.MerchantOnboarding.OnboardingType -> Kernel.Prelude.Text -> m (Maybe Domain.Types.MerchantOnboardingStepConfig.MerchantOnboardingStepConfig))
findByOnboardingTypeAndStepNameIdentifier onboardingType stepNameIdentifier = do findOneWithKV [Se.And [Se.Is Beam.onboardingType $ Se.Eq onboardingType,
                                                                                                        Se.Is Beam.stepNameIdentifier $ Se.Eq stepNameIdentifier]]
findByStepNameIdentifier :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.MerchantOnboardingStepConfig.MerchantOnboardingStepConfig))
findByStepNameIdentifier stepNameIdentifier = do findOneWithKV [Se.Is Beam.stepNameIdentifier $ Se.Eq stepNameIdentifier]
findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                    (Domain.Types.MerchantOnboarding.OnboardingType -> Kernel.Prelude.Text -> m (Maybe Domain.Types.MerchantOnboardingStepConfig.MerchantOnboardingStepConfig))
findByPrimaryKey onboardingType stepNameIdentifier = do findOneWithKV [Se.And [Se.Is Beam.onboardingType $ Se.Eq onboardingType, Se.Is Beam.stepNameIdentifier $ Se.Eq stepNameIdentifier]]
updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MerchantOnboardingStepConfig.MerchantOnboardingStepConfig -> m ())
updateByPrimaryKey (Domain.Types.MerchantOnboardingStepConfig.MerchantOnboardingStepConfig {..}) = do {_now <- getCurrentTime;
                                                                                                       updateWithKV [Se.Set Beam.dependency dependency,
                                                                                                                     Se.Set Beam.isAdminOnly isAdminOnly,
                                                                                                                     Se.Set Beam.isApprovalRequired isApprovalRequired,
                                                                                                                     Se.Set Beam.stepDescription stepDescription,
                                                                                                                     Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.onboardingType $ Se.Eq onboardingType, Se.Is Beam.stepNameIdentifier $ Se.Eq stepNameIdentifier]]}



instance FromTType' Beam.MerchantOnboardingStepConfig Domain.Types.MerchantOnboardingStepConfig.MerchantOnboardingStepConfig
    where fromTType' (Beam.MerchantOnboardingStepConfigT {..}) = do pure $ Just Domain.Types.MerchantOnboardingStepConfig.MerchantOnboardingStepConfig{createdAt = createdAt,
                                                                                                                                                       dependency = dependency,
                                                                                                                                                       isAdminOnly = isAdminOnly,
                                                                                                                                                       isApprovalRequired = isApprovalRequired,
                                                                                                                                                       onboardingType = onboardingType,
                                                                                                                                                       stepDescription = stepDescription,
                                                                                                                                                       stepNameIdentifier = stepNameIdentifier,
                                                                                                                                                       updatedAt = updatedAt}
instance ToTType' Beam.MerchantOnboardingStepConfig Domain.Types.MerchantOnboardingStepConfig.MerchantOnboardingStepConfig
    where toTType' (Domain.Types.MerchantOnboardingStepConfig.MerchantOnboardingStepConfig {..}) = do Beam.MerchantOnboardingStepConfigT{Beam.createdAt = createdAt,
                                                                                                                                         Beam.dependency = dependency,
                                                                                                                                         Beam.isAdminOnly = isAdminOnly,
                                                                                                                                         Beam.isApprovalRequired = isApprovalRequired,
                                                                                                                                         Beam.onboardingType = onboardingType,
                                                                                                                                         Beam.stepDescription = stepDescription,
                                                                                                                                         Beam.stepNameIdentifier = stepNameIdentifier,
                                                                                                                                         Beam.updatedAt = updatedAt}



