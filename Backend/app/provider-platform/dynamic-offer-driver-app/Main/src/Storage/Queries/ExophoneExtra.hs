module Storage.Queries.ExophoneExtra where

-- Extra code goes here --

import qualified Database.Beam as B
import Domain.Types.Exophone as DE (Exophone (..))
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.Exophone as Beam
import qualified Storage.Beam.Exophone as BeamE
import Storage.Queries.OrphanInstances.Exophone ()

updateAffectedPhones :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Text] -> m ()
updateAffectedPhones primaryPhones = do
  now <- getCurrentTime
  dbConf <- getMasterBeamConfig
  let indianMobileCode = "+91"
  void $
    L.runDB dbConf $
      L.updateRows $
        B.update'
          (BeamCommon.exophone BeamCommon.atlasDB)
          ( \BeamE.ExophoneT {..} ->
              ( isPrimaryDown
                  B.<-. ( B.current_ primaryPhone `B.in_` (B.val_ <$> primaryPhones)
                            B.||. (B.concat_ [B.val_ indianMobileCode, B.current_ primaryPhone] `B.in_` (B.val_ <$> primaryPhones))
                        )
              )
                <> (updatedAt B.<-. B.val_ now)
          )
          ( \BeamE.ExophoneT {..} -> do
              isPrimaryDown B.==?. (primaryPhone `B.in_` (B.val_ <$> primaryPhones))
                B.||?. B.sqlBool_ (B.concat_ [B.val_ indianMobileCode, primaryPhone] `B.in_` (B.val_ <$> primaryPhones))
          )

findAllExophones :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => m [Exophone]
findAllExophones = findAllWithDb [Se.Is BeamE.id $ Se.Not $ Se.Eq $ getId ""]

findAllByPhone :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> m [Exophone]
findAllByPhone phone = do
  merchIds <- findAllMerchantIdsByPhone phone
  findAllWithKV [Se.Is BeamE.merchantOperatingCityId $ Se.In $ getId <$> merchIds]

findAllMerchantIdsByPhone :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> m [Id DMOC.MerchantOperatingCity]
findAllMerchantIdsByPhone phone = findAllWithKV [Se.Or [Se.Is BeamE.primaryPhone $ Se.Eq phone, Se.Is BeamE.backupPhone $ Se.Eq phone]] <&> (DE.merchantOperatingCityId <$>)
