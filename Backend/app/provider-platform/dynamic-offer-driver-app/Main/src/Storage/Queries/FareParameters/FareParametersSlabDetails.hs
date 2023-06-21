{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Storage.Queries.FareParameters.FareParametersSlabDetails where

import qualified Domain.Types.FareParameters as Domain
import qualified EulerHS.KVConnector.Flow as KV
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import qualified Kernel.Types.Id as KTI
import Lib.Utils (setMeshConfig)
import Sequelize as Se
import Storage.Beam.FareParameters.FareParametersSlabDetails as BeamFPSD

findById' :: L.MonadFlow m => KTI.Id Domain.FareParameters -> m (Maybe BeamFPSD.FullFareParametersSlabDetails)
findById' (KTI.Id fareParametersId') = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamFPSD.FareParametersSlabDetailsT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' ->
      either (pure Nothing) (transformBeamFareParametersSlabDetailsToDomain <$>)
        <$> KV.findWithKVConnector dbCOnf' updatedMeshConfig [Se.Is fareParametersId $ Se.Eq fareParametersId']
    Nothing -> pure Nothing

transformBeamFareParametersSlabDetailsToDomain :: FareParametersSlabDetails -> BeamFPSD.FullFareParametersSlabDetails
transformBeamFareParametersSlabDetailsToDomain FareParametersSlabDetailsT {..} = do
  ( KTI.Id fareParametersId,
    Domain.FParamsSlabDetails
      { platformFee = platformFee
      }
    )

transformDomainFareParametersSlabDetailsToBeam :: BeamFPSD.FullFareParametersSlabDetails -> FareParametersSlabDetails
transformDomainFareParametersSlabDetailsToBeam (KTI.Id fareParametersId, Domain.FParamsSlabDetails {..}) =
  FareParametersSlabDetailsT
    { fareParametersId = fareParametersId,
      platformFee = platformFee
    }
