{-# OPTIONS_GHC -Wno-orphans -Wno-deprecations #-}

module Storage.ConfigPilot.Config.Exophone
  ( ExophoneDimensions (..),
    filterByService,
  )
where

import qualified Domain.Types.Exophone as DE
import Kernel.External.Call.Types (CallService)
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import qualified Storage.CachedQueries.Exophone as CQExo
import Storage.ConfigPilot.Interface.Getter
import Storage.ConfigPilot.Interface.Types

data ExophoneDimensions = ExophoneDimensions
  { merchantOperatingCityId :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance ConfigTypeInfo 'Exophone where
  type DimensionsFor 'Exophone = ExophoneDimensions
  configTypeValue = Exophone
  sConfigType = SExophone

instance ConfigDimensions ExophoneDimensions where
  type ConfigTypeOf ExophoneDimensions = 'Exophone
  type ConfigValueTypeOf ExophoneDimensions = [DE.Exophone]
  getConfigType _ = Exophone
  getConfig a = do
    cfgs <- CQExo.findAllByMerchantOperatingCityId (Id (merchantOperatingCityId a))
    let configWrapper = LYT.Config {config = cfgs, extraDimensions = Nothing, identifier = 0}
    getConfigImpl a configWrapper (LYT.RIDER_CONFIG Exophone) (Id (merchantOperatingCityId a))

filterByService :: [DE.Exophone] -> CallService -> [DE.Exophone]
filterByService cfgs service = filter (\c -> c.callService == service) cfgs
