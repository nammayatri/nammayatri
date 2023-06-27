{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.SpecialZoneQuote where

import Domain.Types.SpecialZoneQuote
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import Kernel.Types.Id
import Lib.Utils
import qualified Sequelize as Se
import qualified Storage.Beam.SpecialZoneQuote as BeamSZQ

createSpecialZoneQuote :: L.MonadFlow m => SpecialZoneQuote -> m (MeshResult ())
createSpecialZoneQuote specialZoneQuote = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamSZQ.SpecialZoneQuoteT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> KV.createWoReturingKVConnector dbConf' updatedMeshConfig (transformDomainSpecialZoneQuoteToBeam specialZoneQuote)
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

-- findById' :: (MonadThrow m, Log m, Transactionable m) => Id SpecialZoneQuote -> DTypeBuilder m (Maybe SpecialZoneQuoteT)
-- findById' = Esq.findById'

findById :: (L.MonadFlow m) => Id SpecialZoneQuote -> m (Maybe SpecialZoneQuote)
findById specialZoneQuoteId = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamSZQ.SpecialZoneQuoteT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamSpecialZoneQuoteToDomain <$>) <$> KV.findWithKVConnector dbCOnf' updatedMeshConfig [Se.Is BeamSZQ.id $ Se.Eq (getId specialZoneQuoteId)]
    Nothing -> pure Nothing

transformBeamSpecialZoneQuoteToDomain :: BeamSZQ.SpecialZoneQuote -> SpecialZoneQuote
transformBeamSpecialZoneQuoteToDomain BeamSZQ.SpecialZoneQuoteT {..} = do
  SpecialZoneQuote
    { id = Id id,
      quoteId = quoteId
    }

transformDomainSpecialZoneQuoteToBeam :: SpecialZoneQuote -> BeamSZQ.SpecialZoneQuote
transformDomainSpecialZoneQuoteToBeam SpecialZoneQuote {..} =
  BeamSZQ.defaultSpecialZoneQuote
    { BeamSZQ.id = getId id,
      BeamSZQ.quoteId = quoteId
    }
