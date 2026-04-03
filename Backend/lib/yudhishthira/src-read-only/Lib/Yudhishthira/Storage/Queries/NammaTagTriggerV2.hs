{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Lib.Yudhishthira.Storage.Queries.NammaTagTriggerV2 where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Lib.Yudhishthira.Types.NammaTagTriggerV2
import qualified Lib.Yudhishthira.Storage.Beam.NammaTagTriggerV2 as Beam
import qualified Kernel.Prelude
import qualified Lib.Yudhishthira.Types
import qualified Kernel.Types.Id
import qualified Lib.Yudhishthira.Storage.Beam.BeamFlow
import qualified Sequelize as Se



create :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Yudhishthira.Types.NammaTagTriggerV2.NammaTagTriggerV2 -> m ())
create = createWithKV
createMany :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Yudhishthira.Types.NammaTagTriggerV2.NammaTagTriggerV2] -> m ())
createMany = traverse_ create
deleteAllByMerchantOperatingCityIdAndTagName :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) =>
                                                (Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity -> Kernel.Prelude.Text -> m ())
deleteAllByMerchantOperatingCityIdAndTagName merchantOperatingCityId tagName = do deleteWithKV [Se.And [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
                                                                                                        Se.Is Beam.tagName $ Se.Eq tagName]]
findAllByMerchantOperatingCityId :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) =>
                                    (Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity -> m ([Lib.Yudhishthira.Types.NammaTagTriggerV2.NammaTagTriggerV2]))
findAllByMerchantOperatingCityId merchantOperatingCityId = do findAllWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]
findAllByMerchantOperatingCityIdAndEvent :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) =>
                                            (Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity -> Lib.Yudhishthira.Types.ApplicationEvent -> m ([Lib.Yudhishthira.Types.NammaTagTriggerV2.NammaTagTriggerV2]))
findAllByMerchantOperatingCityIdAndEvent merchantOperatingCityId event = do findAllWithKV [Se.And [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
                                                                                                   Se.Is Beam.event $ Se.Eq event]]
findAllByMerchantOperatingCityIdAndTagName :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) =>
                                              (Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity -> Kernel.Prelude.Text -> m ([Lib.Yudhishthira.Types.NammaTagTriggerV2.NammaTagTriggerV2]))
findAllByMerchantOperatingCityIdAndTagName merchantOperatingCityId tagName = do findAllWithKV [Se.And [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
                                                                                                       Se.Is Beam.tagName $ Se.Eq tagName]]
findByPrimaryKey :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) =>
                    (Lib.Yudhishthira.Types.ApplicationEvent -> Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity -> Kernel.Prelude.Text -> m (Maybe Lib.Yudhishthira.Types.NammaTagTriggerV2.NammaTagTriggerV2))
findByPrimaryKey event merchantOperatingCityId tagName = do findOneWithKV [Se.And [Se.Is Beam.event $ Se.Eq event,
                                                                                   Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
                                                                                   Se.Is Beam.tagName $ Se.Eq tagName]]
updateByPrimaryKey :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Yudhishthira.Types.NammaTagTriggerV2.NammaTagTriggerV2 -> m ())
updateByPrimaryKey (Lib.Yudhishthira.Types.NammaTagTriggerV2.NammaTagTriggerV2 {..}) = do {_now <- getCurrentTime;
                                                                                           updateWithKV [Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.event $ Se.Eq event,
                                                                                                                                              Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
                                                                                                                                              Se.Is Beam.tagName $ Se.Eq tagName]]}



instance FromTType' Beam.NammaTagTriggerV2 Lib.Yudhishthira.Types.NammaTagTriggerV2.NammaTagTriggerV2
    where fromTType' (Beam.NammaTagTriggerV2T {..}) = do pure $ Just Lib.Yudhishthira.Types.NammaTagTriggerV2.NammaTagTriggerV2{createdAt = createdAt,
                                                                                                                                event = event,
                                                                                                                                merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
                                                                                                                                tagName = tagName,
                                                                                                                                updatedAt = updatedAt}
instance ToTType' Beam.NammaTagTriggerV2 Lib.Yudhishthira.Types.NammaTagTriggerV2.NammaTagTriggerV2
    where toTType' (Lib.Yudhishthira.Types.NammaTagTriggerV2.NammaTagTriggerV2 {..}) = do Beam.NammaTagTriggerV2T{Beam.createdAt = createdAt,
                                                                                                                  Beam.event = event,
                                                                                                                  Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
                                                                                                                  Beam.tagName = tagName,
                                                                                                                  Beam.updatedAt = updatedAt}



