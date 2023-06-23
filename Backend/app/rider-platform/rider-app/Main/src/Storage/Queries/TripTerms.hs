{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.TripTerms where

import Domain.Types.TripTerms as DTT
import qualified EulerHS.KVConnector.Flow as KV
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import Lib.Utils
import qualified Sequelize as Se
import qualified Storage.Beam.TripTerms as BeamTT
import Storage.Tabular.TripTerms

findById' :: (MonadThrow m, Log m, Transactionable m) => Id TripTerms -> DTypeBuilder m (Maybe TripTermsT)
findById' = Esq.findById'

findById'' :: L.MonadFlow m => Id TripTerms -> m (Maybe TripTerms)
findById'' tripTermsId = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamTT.TripTermsT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamTripTermsToDomain <$>) <$> KV.findWithKVConnector dbCOnf' updatedMeshConfig [Se.Is BeamTT.id $ Se.Eq (getId tripTermsId)]
    Nothing -> pure Nothing

transformBeamTripTermsToDomain :: BeamTT.TripTerms -> TripTerms
transformBeamTripTermsToDomain BeamTT.TripTermsT {..} = do
  TripTerms
    { id = Id id,
      descriptions = DTT.splitDescriptions descriptions
    }

transformDomainTripTermsToBeam :: TripTerms -> BeamTT.TripTerms
transformDomainTripTermsToBeam TripTerms {..} =
  BeamTT.defaultTripTerms
    { BeamTT.id = getId id,
      BeamTT.descriptions = DTT.intercalateDescriptions descriptions
    }
