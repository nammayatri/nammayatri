{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Exophone
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import qualified Database.Beam as B
import Domain.Types.Exophone as DE
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.External.Call.Types (CallService)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.Exophone as BeamE

create :: MonadFlow m => Exophone -> m ()
create = createWithKV

findAllMerchantOperatingCityIdsByPhone :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> m [Id DMOC.MerchantOperatingCity]
findAllMerchantOperatingCityIdsByPhone phone = findAllWithKV [Se.Or [Se.Is BeamE.primaryPhone $ Se.Eq phone, Se.Is BeamE.backupPhone $ Se.Eq phone]] <&> (DE.merchantOperatingCityId <$>)

findAllByPhone :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> m [Exophone]
findAllByPhone phone = do
  merchOpCityIds <- findAllMerchantOperatingCityIdsByPhone phone
  findAllWithKV [Se.Is BeamE.merchantOperatingCityId $ Se.In $ getId <$> merchOpCityIds]

findAllByMerchantOperatingCityId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> m [Exophone]
findAllByMerchantOperatingCityId merchantOperatingCityId = findAllWithKV [Se.Is BeamE.merchantOperatingCityId $ Se.Eq $ getId merchantOperatingCityId]

findAllExophones :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => m [Exophone]
findAllExophones = findAllWithKV [Se.Is BeamE.merchantOperatingCityId $ Se.Not $ Se.Eq ""]

findByMerchantOperatingCityIdAndService :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> CallService -> m [Exophone]
findByMerchantOperatingCityIdAndService merchantOperatingCityId service = findAllWithKV [Se.And [Se.Is BeamE.merchantOperatingCityId $ Se.Eq merchantOperatingCityId.getId, Se.Is BeamE.callService $ Se.Eq service]]

updateAffectedPhones :: MonadFlow m => [Text] -> m ()
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

deleteByMerchantOperatingCityId :: MonadFlow m => Id DMOC.MerchantOperatingCity -> m ()
deleteByMerchantOperatingCityId (Id merchantOperatingCityId) = deleteWithKV [Se.Is BeamE.merchantOperatingCityId (Se.Eq merchantOperatingCityId)]

instance FromTType' BeamE.Exophone Exophone where
  fromTType' BeamE.ExophoneT {..} = do
    pure $
      Just
        Exophone
          { id = Id id,
            -- merchantId = Id merchantId,
            merchantOperatingCityId = Id merchantOperatingCityId,
            primaryPhone = primaryPhone,
            backupPhone = backupPhone,
            isPrimaryDown = isPrimaryDown,
            callService = callService,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' BeamE.Exophone Exophone where
  toTType' Exophone {..} = do
    BeamE.ExophoneT
      { BeamE.id = getId id,
        -- BeamE.merchantId = getId merchantId,
        BeamE.merchantOperatingCityId = getId merchantOperatingCityId,
        BeamE.primaryPhone = primaryPhone,
        BeamE.backupPhone = backupPhone,
        BeamE.isPrimaryDown = isPrimaryDown,
        BeamE.callService = callService,
        BeamE.createdAt = createdAt,
        BeamE.updatedAt = updatedAt
      }
