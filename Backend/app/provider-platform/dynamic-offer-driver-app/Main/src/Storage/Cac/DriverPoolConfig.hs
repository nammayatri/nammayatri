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

module Storage.Cac.DriverPoolConfig (module Storage.Cac.DriverPoolConfig, module Reexport) where

import qualified Client.Main as CM
import Data.Aeson as DA
import Data.Text as Text hiding (find)
import qualified Domain.Types.Cac as DTC
import qualified Domain.Types.Common as DTC
import Domain.Types.DriverPoolConfig
import Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.ServiceTierType as DVST
import Kernel.Beam.Functions as KBF
import Kernel.Prelude as KP
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Cac
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow)
import Kernel.Utils.Error
import Kernel.Utils.Logging
import qualified Lib.Types.SpecialLocation as SL
import SharedLogic.DriverPool.Config as DPC
import SharedLogic.DriverPool.Types as Reexport
import qualified Storage.Beam.DriverPoolConfig as SBMDPC
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.Merchant.DriverPoolConfig as CDP
import Storage.Queries.DriverPoolConfig ()
import Utils.Common.CacUtils as CCU

getSearchDriverPoolConfig ::
  KvDbFlow m r =>
  Id MerchantOperatingCity ->
  Maybe Meters ->
  SL.Area ->
  m (Maybe DriverPoolConfig)
getSearchDriverPoolConfig merchantOpCityId mbDist area = do
  let serviceTier = Nothing
      tripCategory = "All"
  getdriverPoolConfigCond merchantOpCityId serviceTier tripCategory mbDist area Nothing

getDriverPoolConfigFromCAC ::
  KvDbFlow m r =>
  Id MerchantOperatingCity ->
  Maybe DVST.ServiceTierType ->
  String ->
  Meters ->
  SL.Area ->
  Maybe CacKey ->
  m (Maybe DriverPoolConfig)
getDriverPoolConfigFromCAC merchantOpCityId st tc dist area stickyKey = do
  let dpcCond =
        [ (MerchantOperatingCityId, toJSON (getId merchantOpCityId)),
          (TripDistance, toJSON (getMeters dist)),
          (TripCategory, toJSON tc),
          (Area, show area)
        ]
          <> [(VehicleVariant, show (fromJust st)) | isJust st]
  inMemConfig <- getConfigFromInMemory merchantOpCityId st tc dist
  config <-
    CCU.getConfigFromCacOrDB inMemConfig dpcCond stickyKey (KBF.fromCacType @SBMDPC.DriverPoolConfig) CCU.DriverPoolConfig
      |<|>| ( do
                logDebug $ "DriverPoolConfig not found in memory, fetching from DB for context: " <> show dpcCond
                DPC.getDriverPoolConfigFromDB merchantOpCityId st tc area (Just dist)
            )
  setConfigInMemory merchantOpCityId st tc dist config

doubleToInt :: Double -> Int
doubleToInt = floor

getdriverPoolConfigCond ::
  KvDbFlow m r =>
  Id MerchantOperatingCity ->
  Maybe DVST.ServiceTierType ->
  String ->
  Maybe Meters ->
  SL.Area ->
  Maybe CacKey ->
  m (Maybe DriverPoolConfig)
getdriverPoolConfigCond merchantOpCityId serviceTier tripCategory dist' area stickeyKey = do
  let dist = fromMaybe 0 dist'
  getDriverPoolConfigFromCAC merchantOpCityId serviceTier tripCategory dist area stickeyKey
    |<|>| getDriverPoolConfigFromCAC merchantOpCityId serviceTier tripCategory dist SL.Default stickeyKey
    |<|>| getDriverPoolConfigFromCAC merchantOpCityId serviceTier "All" dist area stickeyKey
    |<|>| getDriverPoolConfigFromCAC merchantOpCityId serviceTier "All" dist SL.Default stickeyKey
    |<|>| getDriverPoolConfigFromCAC merchantOpCityId Nothing "All" dist area stickeyKey
    |<|>| getDriverPoolConfigFromCAC merchantOpCityId Nothing "All" dist SL.Default stickeyKey

-- TODO :: Need To Handle `area` Properly In CAC
getConfigFromInMemory ::
  KvDbFlow m r =>
  Id MerchantOperatingCity ->
  Maybe DVST.ServiceTierType ->
  String ->
  Meters ->
  m (Maybe DriverPoolConfig)
getConfigFromInMemory id mbvst tripCategory dist = do
  let roundeDist = doubleToInt (fromIntegral (dist.getMeters) / 1000)
  isExpired <- DTC.updateConfig DTC.LastUpdatedDriverPoolConfig
  getConfigFromMemoryCommon (DTC.DriverPoolConfig id.getId (show mbvst) tripCategory roundeDist) isExpired CM.isExperimentsRunning

setConfigInMemory ::
  KvDbFlow m r =>
  Id MerchantOperatingCity ->
  Maybe DVST.ServiceTierType ->
  String ->
  Meters ->
  Maybe DriverPoolConfig ->
  m (Maybe DriverPoolConfig)
setConfigInMemory id mbvst tripCategory dist config = do
  let roundeDist = doubleToInt (fromIntegral (dist.getMeters) / 1000)
  isExp <- DTC.inMemConfigUpdateTime DTC.LastUpdatedDriverPoolConfig
  CCU.setConfigInMemoryCommon (DTC.DriverPoolConfig id.getId (show mbvst) tripCategory roundeDist) isExp config

getDriverPoolConfig ::
  KvDbFlow m r =>
  Id MerchantOperatingCity ->
  DVST.ServiceTierType ->
  DTC.TripCategory ->
  SL.Area ->
  Maybe Meters ->
  Maybe CacKey ->
  m DriverPoolConfig
getDriverPoolConfig merchantOpCityId serviceTier tripCategory area tripDistance srId = do
  config <- getdriverPoolConfigCond merchantOpCityId (Just serviceTier) (show tripCategory) tripDistance area srId
  when (isNothing config) do
    logError $ "Could not find the config for merchantOpCityId:" <> getId merchantOpCityId <> " and serviceTier:" <> show serviceTier <> " and tripCategory:" <> show tripCategory <> " and tripDistance:" <> show tripDistance
    throwError $ InvalidRequest $ "DriverPool Configs not found for MerchantOperatingCity: " <> merchantOpCityId.getId
  logDebug $ "driverPoolConfig we recieved for merchantOpCityId:" <> getId merchantOpCityId <> " and serviceTier:" <> show serviceTier <> " and tripCategory:" <> show tripCategory <> " and tripDistance:" <> show tripDistance <> " is:" <> show config
  pure (fromJust config)

----------------------------------------------- Cached Queries Can't be handled by cac ------------------------------------------

create :: KvDbFlow m r => DriverPoolConfig -> m ()
create = CDP.create

findAllByMerchantOpCityId :: KvDbFlow m r => Id MerchantOperatingCity -> m [DriverPoolConfig]
findAllByMerchantOpCityId = CDP.findAllByMerchantOpCityId

findByMerchantOpCityIdAndTripDistance :: KvDbFlow m r => Id MerchantOperatingCity -> Meters -> m (Maybe DriverPoolConfig)
findByMerchantOpCityIdAndTripDistance = CDP.findByMerchantOpCityIdAndTripDistance

findByMerchantOpCityIdAndTripDistanceAndAreaAndDVeh :: KvDbFlow m r => Id MerchantOperatingCity -> Meters -> Maybe DVST.ServiceTierType -> Text -> SL.Area -> m (Maybe DriverPoolConfig)
findByMerchantOpCityIdAndTripDistanceAndAreaAndDVeh = CDP.findByMerchantOpCityIdAndTripDistanceAndAreaAndDVeh

-- Call it after any update
clearCache :: Hedis.HedisFlow m r => Id MerchantOperatingCity -> m ()
clearCache = CDP.clearCache

update :: KvDbFlow m r => DriverPoolConfig -> m ()
update = CDP.update

instance FromCacType SBMDPC.DriverPoolConfig DriverPoolConfig where
  fromCacType = fromTType'
