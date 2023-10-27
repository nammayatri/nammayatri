{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Merchant.Overlay
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.Merchant.MerchantOperatingCity
import Domain.Types.Merchant.Overlay
import Kernel.Beam.Functions
import Kernel.External.Types (Language)
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Merchant.Overlay as BeamMPN

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Overlay -> m ()
create = createWithKV

createMany :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Overlay] -> m ()
createMany = traverse_ create

deleteByOverlayKeyMerchantIdUdf :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id MerchantOperatingCity -> Text -> Maybe Text -> m ()
deleteByOverlayKeyMerchantIdUdf merchantOperatingCityId overlayKey udf1 =
  deleteWithKV
    [ Se.And
        [ Se.Is BeamMPN.overlayKey (Se.Eq overlayKey),
          Se.Is BeamMPN.merchantOperatingCityId $ Se.Eq (getId merchantOperatingCityId),
          Se.Is BeamMPN.udf1 $ Se.Eq udf1
        ]
    ]

findAllByLanguage :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id MerchantOperatingCity -> Language -> m [Overlay]
findAllByLanguage merchantOperatingCityId language =
  findAllWithKV
    [ Se.And
        [ Se.Is BeamMPN.merchantOperatingCityId $ Se.Eq (getId merchantOperatingCityId),
          Se.Is BeamMPN.language $ Se.Eq language
        ]
    ]

findAllByOverlayKeyUdf :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id MerchantOperatingCity -> Text -> Maybe Text -> m [Overlay]
findAllByOverlayKeyUdf merchantOperatingCityId overlayKey udf1 =
  findAllWithKV
    [ Se.And
        [ Se.Is BeamMPN.merchantOperatingCityId $ Se.Eq (getId merchantOperatingCityId),
          Se.Is BeamMPN.overlayKey $ Se.Eq overlayKey,
          Se.Is BeamMPN.udf1 $ Se.Eq udf1
        ]
    ]

findByMerchantOpCityIdPNKeyLangaugeUdf :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id MerchantOperatingCity -> Text -> Language -> Maybe Text -> m (Maybe Overlay)
findByMerchantOpCityIdPNKeyLangaugeUdf id pnKey language udf1 =
  findOneWithKV
    [ Se.And
        [ Se.Is BeamMPN.merchantOperatingCityId $ Se.Eq (getId id),
          Se.Is BeamMPN.overlayKey $ Se.Eq pnKey,
          Se.Is BeamMPN.language $ Se.Eq language,
          Se.Is BeamMPN.udf1 $ Se.Eq udf1
        ]
    ]

instance FromTType' BeamMPN.Overlay Overlay where
  fromTType' BeamMPN.OverlayT {..} = do
    pure $
      Just
        Overlay
          { id = Id id,
            merchantId = Id merchantId,
            merchantOperatingCityId = Id merchantOperatingCityId,
            ..
          }

instance ToTType' BeamMPN.Overlay Overlay where
  toTType' Overlay {..} = do
    BeamMPN.OverlayT
      { BeamMPN.id = getId id,
        BeamMPN.merchantId = getId merchantId,
        BeamMPN.merchantOperatingCityId = getId merchantOperatingCityId,
        BeamMPN.overlayKey = overlayKey,
        BeamMPN.language = language,
        BeamMPN.udf1 = udf1,
        BeamMPN.title = title,
        BeamMPN.description = description,
        BeamMPN.imageUrl = imageUrl,
        BeamMPN.okButtonText = okButtonText,
        BeamMPN.cancelButtonText = cancelButtonText,
        BeamMPN.actions = actions,
        BeamMPN.link = link,
        BeamMPN.method = method,
        BeamMPN.reqBody = reqBody,
        BeamMPN.endPoint = endPoint
      }
