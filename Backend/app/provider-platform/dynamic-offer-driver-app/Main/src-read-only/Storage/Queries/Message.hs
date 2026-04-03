{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.Message (module Storage.Queries.Message, module ReExport) where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import Storage.Queries.MessageExtra as ReExport
import qualified Domain.Types.Message
import qualified Storage.Beam.Message as Beam
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Sequelize as Se



updateShareable :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.Message.Message -> m ())
updateShareable shareable id = do updateOneWithKV [Se.Set Beam.shareable (Kernel.Prelude.Just shareable)] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]
findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Message.Message -> m (Maybe Domain.Types.Message.Message))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Message.Message -> m ())
updateByPrimaryKey (Domain.Types.Message.Message {..}) = do updateWithKV [Se.Set Beam.messageType (_type),
                                                                          Se.Set Beam.alwaysTriggerOnOnboarding (Kernel.Prelude.Just alwaysTriggerOnOnboarding),
                                                                          Se.Set Beam.description description,
                                                                          Se.Set Beam.label label,
                                                                          Se.Set Beam.likeCount likeCount,
                                                                          Se.Set Beam.mediaFiles (Kernel.Types.Id.getId <$> mediaFiles),
                                                                          Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
                                                                          Se.Set Beam.merchantOperatingCityId (Kernel.Prelude.Just (Kernel.Types.Id.getId merchantOperatingCityId)),
                                                                          Se.Set Beam.shareable (Kernel.Prelude.Just shareable),
                                                                          Se.Set Beam.shortDescription shortDescription,
                                                                          Se.Set Beam.title title,
                                                                          Se.Set Beam.viewCount viewCount] [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]



