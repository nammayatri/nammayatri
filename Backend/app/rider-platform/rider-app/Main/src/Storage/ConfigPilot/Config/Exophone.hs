{-# OPTIONS_GHC -Wno-orphans -Wno-deprecations #-}

module Storage.ConfigPilot.Config.Exophone
  ( ExophoneDimensions (..),
  )
where

import qualified Domain.Types.Exophone as DE
import Kernel.External.Call.Types (CallService)
import Kernel.Prelude
import qualified Kernel.Storage.InMem as IM
import Kernel.Types.Id
import qualified Lib.ConfigPilot.Interface.Getter as LibCPGetter
import Lib.ConfigPilot.Interface.Types
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.Exophone as CQExo

data ExophoneDimensions = ExophoneDimensions
  { merchantOperatingCityId :: Text,
    callService :: Maybe CallService
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

instance ConfigTypeInfo 'Exophone where
  type DimensionsFor 'Exophone = ExophoneDimensions
  configTypeValue = Exophone
  sConfigType = SExophone

instance ConfigDimensions ExophoneDimensions where
  type ConfigTypeOf ExophoneDimensions = 'Exophone
  type ConfigValueTypeOf ExophoneDimensions = [DE.Exophone]
  getConfigType _ = Exophone
  getConfigList a = do
    let mocId = a.merchantOperatingCityId
    IM.withInMemCache (LibCPGetter.configPilotInMemKey a) 3600 $ do
      cfgs <- CQExo.findAllByMerchantOperatingCityId (Id mocId)
      let filtered = filterByDimensions a cfgs
      let configWrappers = map (\cfg -> LYT.Config {config = cfg, extraDimensions = Nothing, identifier = 0}) filtered
      mapM (\configWrapper -> LibCPGetter.getConfigImpl a configWrapper (LYT.RIDER_CONFIG Exophone) (Id mocId)) configWrappers
  filterByDimensions dims cfgs = filter matchesDims cfgs
    where
      matchesDims c =
        maybe True (\cs -> c.callService == cs) dims.callService
