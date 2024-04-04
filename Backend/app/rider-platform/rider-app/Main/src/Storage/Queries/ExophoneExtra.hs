{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.ExophoneExtra where

import qualified Database.Beam as B
import qualified Domain.Types.Exophone
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import qualified Kernel.External.Call.Types
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.Exophone as Beam
import Storage.Queries.OrphanInstances.Exophone

-- Extra code goes here --

updateAffectedPhones :: KvDbFlow m r => [Text] -> m ()
updateAffectedPhones primaryPhones = do
  now <- getCurrentTime
  dbConf <- getMasterBeamConfig
  let indianMobileCode = "+91"
  void $
    L.runDB dbConf $
      L.updateRows $
        B.update'
          (BeamCommon.exophone BeamCommon.atlasDB)
          ( \Beam.ExophoneT {..} ->
              ( isPrimaryDown
                  B.<-. ( B.current_ primaryPhone `B.in_` (B.val_ <$> primaryPhones)
                            B.||. (B.concat_ [B.val_ indianMobileCode, B.current_ primaryPhone] `B.in_` (B.val_ <$> primaryPhones))
                        )
              )
                <> (updatedAt B.<-. B.val_ now)
          )
          ( \Beam.ExophoneT {..} -> do
              isPrimaryDown B.==?. (primaryPhone `B.in_` (B.val_ <$> primaryPhones))
                B.||?. B.sqlBool_ (B.concat_ [B.val_ indianMobileCode, primaryPhone] `B.in_` (B.val_ <$> primaryPhones))
          )

findAllExophones :: KvDbFlow m r => m [Domain.Types.Exophone.Exophone]
findAllExophones = findAllWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Not $ Se.Eq ""]

findAllMerchantOperatingCityIdsByPhone :: KvDbFlow m r => Text -> m [Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity]
findAllMerchantOperatingCityIdsByPhone phone = findAllWithKV [Se.Or [Se.Is Beam.primaryPhone $ Se.Eq phone, Se.Is Beam.backupPhone $ Se.Eq phone]] <&> (Domain.Types.Exophone.merchantOperatingCityId <$>)

findAllByPhone :: KvDbFlow m r => Text -> m [Domain.Types.Exophone.Exophone]
findAllByPhone phone = do
  merchOpCityIds <- findAllMerchantOperatingCityIdsByPhone phone
  findAllWithKV [Se.Is Beam.merchantOperatingCityId $ Se.In $ Kernel.Types.Id.getId <$> merchOpCityIds]
