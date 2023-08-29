{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.RegistryMapFallback where

import Domain.Types.RegistryMapFallback
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import qualified Sequelize as Se
import qualified Storage.Beam.RegistryMapFallback as BeamRMT

findBySubscriberId :: MonadFlow m => Text -> m [RegistryMapFallback]
findBySubscriberId subscriberId = findAllWithKV [Se.Is BeamRMT.subscriberId $ Se.Eq subscriberId]

findByUniqueId :: MonadFlow m => Text -> m [RegistryMapFallback]
findByUniqueId uniqueId = findAllWithKV [Se.Is BeamRMT.uniqueId $ Se.Eq uniqueId]

findBySubscriberIdAndUniqueId :: MonadFlow m => Text -> Text -> m (Maybe RegistryMapFallback)
findBySubscriberIdAndUniqueId subscriberId uniqueId = findOneWithKV [Se.And [Se.Is BeamRMT.subscriberId $ Se.Eq subscriberId, Se.Is BeamRMT.uniqueId $ Se.Eq uniqueId]]

instance FromTType' BeamRMT.RegistryMapFallback RegistryMapFallback where
  fromTType' BeamRMT.RegistryMapFallbackT {..} = do
    regUrl <- parseBaseUrl registryUrl
    pure $
      Just
        RegistryMapFallback
          { subscriberId = subscriberId,
            uniqueId = uniqueId,
            registryUrl = regUrl
          }

instance ToTType' BeamRMT.RegistryMapFallback RegistryMapFallback where
  toTType' RegistryMapFallback {..} = do
    BeamRMT.RegistryMapFallbackT
      { BeamRMT.subscriberId = subscriberId,
        BeamRMT.uniqueId = uniqueId,
        BeamRMT.registryUrl = showBaseUrl registryUrl
      }
