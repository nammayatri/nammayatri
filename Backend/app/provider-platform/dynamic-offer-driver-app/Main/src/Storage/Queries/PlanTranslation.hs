{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.PlanTranslation where

import qualified Domain.Types.Plan as Plan
import Domain.Types.PlanTranslation
import Kernel.Beam.Functions
import Kernel.External.Types (Language)
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.PlanTranslation as BeamPT

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => PlanTranslation -> m ()
create = createWithKV

findByPlanIdAndLanguage :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Plan.Plan -> Language -> m (Maybe PlanTranslation)
findByPlanIdAndLanguage (Id planId) language = findOneWithKV [Se.And [Se.Is BeamPT.planId $ Se.Eq planId, Se.Is BeamPT.language $ Se.Eq language]]

instance FromTType' BeamPT.PlanTranslation PlanTranslation where
  fromTType' BeamPT.PlanTranslationT {..} = do
    pure $
      Just
        PlanTranslation
          { planId = Id planId,
            language = language,
            name = name,
            description = description
          }

instance ToTType' BeamPT.PlanTranslation PlanTranslation where
  toTType' PlanTranslation {..} = do
    BeamPT.PlanTranslationT
      { BeamPT.planId = getId planId,
        BeamPT.language = language,
        BeamPT.name = name,
        BeamPT.description = description
      }
