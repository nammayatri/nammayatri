{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.EstimateRevised where

-- import Data.Coerce (coerce)
import Domain.Types.Common
import Domain.Types.EstimateRevised as Domain
-- import Domain.Types.Estimate as DEst
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.EstimateRevised as BeamER
import qualified Storage.CachedQueries.FarePolicy as BeamQFPolicy
import qualified Storage.Queries.FareParameters as BeamQFP

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Domain.EstimateRevised -> m ()
create estimateRevised = do
  case estimateRevised.fareParams of
    Just params -> BeamQFP.create params
    Nothing -> return ()
  createWithKV estimateRevised

createMany :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [EstimateRevised] -> m ()
createMany = traverse_ create

findById :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id EstimateRevised -> m (Maybe EstimateRevised)
findById (Id estimateId) = findOneWithKV [Se.Is BeamER.id $ Se.Eq estimateId]

instance FromTType' BeamER.EstimateRevised EstimateRevised where
  fromTType' BeamER.EstimateRevisedT {..} = do
    farePolicy <- maybe (pure Nothing) (BeamQFPolicy.findById . Id) farePolicyId
    fareParams <- maybe (pure Nothing) (BeamQFP.findById . Id) fareParamsId
    pure $
      Just
        EstimateRevised
          { id = Id id,
            requestId = Id requestId,
            tripCategory = fromMaybe (OneWay OneWayOnDemandDynamicOffer) tripCategory,
            isScheduled = fromMaybe False isScheduled,
            updatedAt = fromMaybe createdAt updatedAt, -- backward compatibility
            -- convienceFee = convienceFee
            ..
          }

instance ToTType' BeamER.EstimateRevised EstimateRevised where
  toTType' EstimateRevised {..} = do
    BeamER.EstimateRevisedT
      { id = getId id,
        requestId = getId requestId,
        tripCategory = Just tripCategory,
        farePolicyId = (getId . (.id)) <$> farePolicy,
        fareParamsId = (getId . (.id)) <$> fareParams,
        isScheduled = Just isScheduled,
        updatedAt = Just updatedAt,
        ..
      }

-- convienceFee = convienceFee
