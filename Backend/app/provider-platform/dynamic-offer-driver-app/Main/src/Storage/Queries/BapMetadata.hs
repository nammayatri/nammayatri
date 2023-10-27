{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.BapMetadata where

import Domain.Types.BapMetadata
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.BapMetadata as BeamBMD

findById :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id BapMetadata -> m (Maybe BapMetadata)
findById (Id metaDataId) = findOneWithKV [Se.Is BeamBMD.id $ Se.Eq metaDataId]

instance FromTType' BeamBMD.BapMetadata BapMetadata where
  fromTType' BeamBMD.BapMetadataT {..} = do
    logoUrl_ <- parseBaseUrl logoUrl
    pure $
      Just
        BapMetadata
          { id = Id id,
            name = name,
            logoUrl = logoUrl_
          }

instance ToTType' BeamBMD.BapMetadata BapMetadata where
  toTType' BapMetadata {..} = do
    BeamBMD.BapMetadataT
      { BeamBMD.id = getId id,
        BeamBMD.name = name,
        BeamBMD.logoUrl = showBaseUrl logoUrl
      }
