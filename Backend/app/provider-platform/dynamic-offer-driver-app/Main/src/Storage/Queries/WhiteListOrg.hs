{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.WhiteListOrg
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.WhiteListOrg
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Types.Registry.Subscriber (Subscriber)
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.WhiteListOrg as BeamBLO

findBySubscriberId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => ShortId Subscriber -> m (Maybe WhiteListOrg)
findBySubscriberId subscriberId = findOneWithKV [Se.Is BeamBLO.subscriberId $ Se.Eq $ getShortId subscriberId]

instance FromTType' BeamBLO.WhiteListOrg WhiteListOrg where
  fromTType' BeamBLO.WhiteListOrgT {..} = do
    pure $
      Just
        WhiteListOrg
          { id = Id id,
            subscriberId = ShortId subscriberId,
            _type = orgType
          }

instance ToTType' BeamBLO.WhiteListOrg WhiteListOrg where
  toTType' WhiteListOrg {..} = do
    BeamBLO.WhiteListOrgT
      { BeamBLO.id = getId id,
        BeamBLO.subscriberId = getShortId subscriberId,
        BeamBLO.orgType = _type
      }
