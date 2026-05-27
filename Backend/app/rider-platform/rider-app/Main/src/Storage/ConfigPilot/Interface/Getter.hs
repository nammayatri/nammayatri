-- | App-level wrapper for getConfigImpl.
-- Imports orphan HasSchemaName instances so that the Yudhishthira query
-- functions can be passed to the library's getConfigImplWith without
-- requiring BeamFlow at the class-method level.
module Storage.ConfigPilot.Interface.Getter
  ( getConfigImpl,
    module Reexport,
  )
where

import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import Lib.ConfigPilot.Interface.Getter as Reexport (PersonIdKey (..), TxnIdKey (..), configPilotInMemKey, invalidateConfigInMem)
import qualified Lib.ConfigPilot.Interface.Getter as LibGetter
import Lib.ConfigPilot.Interface.Types (ConfigDimensions)
import qualified Lib.Yudhishthira.Storage.CachedQueries.AppDynamicLogicRollout as CADLR
import qualified Lib.Yudhishthira.Storage.Queries.AppDynamicLogicElementExtra as CADLE
import qualified Lib.Yudhishthira.Types as LYT
import Storage.Beam.Yudhishthira ()

getConfigImpl ::
  forall configTypeDimensions b m r.
  (ConfigDimensions configTypeDimensions, FromJSON b, ToJSON b, MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  configTypeDimensions ->
  LYT.Config b ->
  LYT.LogicDomain ->
  Id LYT.MerchantOperatingCity ->
  m b
getConfigImpl = LibGetter.getConfigImplWith CADLR.findActiveByMerchantOpCityAndDomain CADLE.findByDomainAndVersions
