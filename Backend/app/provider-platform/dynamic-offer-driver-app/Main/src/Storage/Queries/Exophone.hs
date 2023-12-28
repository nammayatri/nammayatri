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
import Domain.Types.Exophone as DE (Exophone (..), ExophoneType (..))
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.External.Call.Types (CallService)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.Exophone as BeamE

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => DE.Exophone -> m ()
create = createWithKV

findAllMerchantIdsByPhone :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> m [Id DMOC.MerchantOperatingCity]
findAllMerchantIdsByPhone phone = findAllWithKV [Se.Or [Se.Is BeamE.primaryPhone $ Se.Eq phone, Se.Is BeamE.backupPhone $ Se.Eq phone]] <&> (DE.merchantOperatingCityId <$>)

findAllByPhone :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> m [Exophone]
findAllByPhone phone = do
  merchIds <- findAllMerchantIdsByPhone phone
  findAllWithKV [Se.Is BeamE.merchantOperatingCityId $ Se.In $ getId <$> merchIds]

findAllByMerchantOpCityId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> m [Exophone]
findAllByMerchantOpCityId (Id merchantOpCityId) = do
  findAllWithKV [Se.Is BeamE.merchantOperatingCityId $ Se.Eq merchantOpCityId]

findAllExophones :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => m [Exophone]
findAllExophones = findAllWithDb [Se.Is BeamE.id $ Se.Not $ Se.Eq $ getId ""]

findByMerchantOpCityIdServiceAndExophoneType :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> CallService -> DE.ExophoneType -> m [Exophone]
findByMerchantOpCityIdServiceAndExophoneType merchantOpCityId service exophoneType = findAllWithKV [Se.And [Se.Is BeamE.merchantOperatingCityId $ Se.Eq merchantOpCityId.getId, Se.Is BeamE.callService $ Se.Eq service, Se.Is BeamE.exophoneType $ Se.Eq exophoneType]]

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

deleteByMerchantOpCityId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> m ()
deleteByMerchantOpCityId (Id merchantOpCityId) = deleteWithKV [Se.Is BeamE.merchantOperatingCityId (Se.Eq merchantOpCityId)]

instance FromTType' BeamE.Exophone Exophone where
  fromTType' BeamE.ExophoneT {..} = do
    pure $
      Just
        Exophone
          { id = Id id,
            merchantId = Id merchantId,
            merchantOperatingCityId = Id merchantOperatingCityId,
            primaryPhone = primaryPhone,
            backupPhone = backupPhone,
            isPrimaryDown = isPrimaryDown,
            exophoneType = exophoneType,
            callService = callService,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' BeamE.Exophone Exophone where
  toTType' Exophone {..} = do
    BeamE.ExophoneT
      { BeamE.id = getId id,
        BeamE.merchantId = getId merchantId,
        BeamE.merchantOperatingCityId = getId merchantOperatingCityId,
        BeamE.primaryPhone = primaryPhone,
        BeamE.backupPhone = backupPhone,
        BeamE.isPrimaryDown = isPrimaryDown,
        BeamE.exophoneType = exophoneType,
        BeamE.callService = callService,
        BeamE.createdAt = createdAt,
        BeamE.updatedAt = updatedAt
      }
