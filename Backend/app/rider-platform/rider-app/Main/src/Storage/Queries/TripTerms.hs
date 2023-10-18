{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.TripTerms where

import Domain.Types.TripTerms as DTT
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.TripTerms as BeamTT

createTripTerms :: MonadFlow m => TripTerms -> m ()
createTripTerms = createWithKV

findById'' :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id TripTerms -> m (Maybe TripTerms)
findById'' tripTermsId = findOneWithKV [Se.Is BeamTT.id $ Se.Eq (getId tripTermsId)]

instance FromTType' BeamTT.TripTerms TripTerms where
  fromTType' BeamTT.TripTermsT {..} = do
    pure $
      Just
        TripTerms
          { id = Id id,
            descriptions = DTT.splitDescriptions descriptions
          }

instance ToTType' BeamTT.TripTerms TripTerms where
  toTType' TripTerms {..} = do
    BeamTT.TripTermsT
      { BeamTT.id = getId id,
        BeamTT.descriptions = DTT.intercalateDescriptions descriptions
      }
