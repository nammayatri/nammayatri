{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Cac.FarePolicy where

import qualified Client.Main as CM
import qualified Data.Aeson as DA
import qualified Domain.Types.Cac as DTC
import Domain.Types.FarePolicy as Domain
import Kernel.Prelude
import Kernel.Storage.Hedis
import Kernel.Types.Cac
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Beam.FarePolicy as BeamFP
import Storage.Beam.SystemConfigs ()
import qualified Storage.Cac.FarePolicy.DriverExtraFeeBounds as CQueriesDEFB
import qualified Storage.Cac.FarePolicy.FarePolicyProgressiveDetails as CQueriesFPPD
import qualified Storage.Cac.FarePolicy.FarePolicyRentalDetails as CQueriesFPRD
import qualified Storage.Cac.FarePolicy.FarePolicySlabsDetails.FarePolicySlabsDetailsSlab as CQueriesFPSDS
import qualified Storage.CachedQueries.FarePolicy as SCQF
import Storage.Queries.FarePolicy (FarePolicyHandler (..), fromTTypeFarePolicy)
import Utils.Common.CacUtils as CCU

fromCacTypeCustom :: KvDbFlow m r => ([(CacContext, Value)], String, Id FarePolicy, Int) -> BeamFP.FarePolicy -> m (Maybe FarePolicy)
fromCacTypeCustom (context, tenant, id', toss) fp = fromCacType (fp, context, tenant, id', toss)

getConfigFromInMemory :: KvDbFlow m r => Id FarePolicy -> m (Maybe FarePolicy)
getConfigFromInMemory id = do
  isExp <- DTC.updateConfig DTC.LastUpdatedFarePolicy
  getConfigFromMemoryCommon (DTC.FarePolicy id.getId) isExp CM.isExperimentsRunning

setConfigInMemory :: KvDbFlow m r => Id FarePolicy -> Maybe FarePolicy -> m (Maybe FarePolicy)
setConfigInMemory id config = do
  isExp <- DTC.inMemConfigUpdateTime DTC.LastUpdatedFarePolicy
  CCU.setConfigInMemoryCommon (DTC.FarePolicy id.getId) isExp config

findById :: KvDbFlow m r => Maybe CacKey -> Id FarePolicy -> m (Maybe FarePolicy)
findById stickeyKey id = do
  let context = [(FarePolicyId, DA.toJSON id.getId)]
  inMemConfig <- getConfigFromInMemory id
  tenant <- asks (.cacConfig.tenant)
  toss <- getToss (getKeyValue <$> stickeyKey)
  config <-
    getConfigFromCacOrDB inMemConfig context stickeyKey (fromCacTypeCustom (context, tenant, id, toss)) CCU.FarePolicy
      |<|>| ( do
                logDebug $ "FarePolicy not found in memory, fetching from DB for context: " <> show context
                SCQF.findFarePolicyFromDB id
            )
  setConfigInMemory id config

-- Call it after any update
clearCache :: HedisFlow m r => FarePolicy -> m ()
clearCache = SCQF.clearCache

clearCacheById :: HedisFlow m r => Id FarePolicy -> m ()
clearCacheById = SCQF.clearCacheById

update :: KvDbFlow m r => FarePolicy -> m ()
update = SCQF.update

update' :: KvDbFlow m r => FarePolicy -> m ()
update' = SCQF.update'

instance FromCacType (BeamFP.FarePolicy, [(CacContext, Value)], String, Id FarePolicy, Int) Domain.FarePolicy where
  fromCacType farePolicyCac@(farePolicyT, _, _, _, _) = fromTTypeFarePolicy (mkCacFarePolicyHandler farePolicyCac) farePolicyT

mkCacFarePolicyHandler ::
  KvDbFlow m r =>
  (BeamFP.FarePolicy, [(CacContext, Value)], String, Id FarePolicy, Int) ->
  FarePolicyHandler m
mkCacFarePolicyHandler (_farePolicy, context, tenant, id', toss) =
  FarePolicyHandler
    { findAllDriverExtraFeeBounds = CQueriesDEFB.getDriverExtraFeeBoundsFromCAC context tenant id' toss,
      findProgressiveDetails = CQueriesFPPD.getFPProgressiveDetailsFromCAC context tenant id' toss,
      findAllSlabDetailsSlabs = CQueriesFPSDS.getFarePolicySlabsDetailsSlabFromCAC context tenant id' toss,
      findRentalDetails = CQueriesFPRD.findFarePolicyRentalDetailsFromCAC context tenant id' toss
    }
