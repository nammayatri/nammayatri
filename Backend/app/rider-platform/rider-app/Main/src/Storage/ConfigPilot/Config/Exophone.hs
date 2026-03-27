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
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import qualified Storage.CachedQueries.Exophone as CQExo
import Storage.ConfigPilot.Interface.Getter
import Storage.ConfigPilot.Interface.Types

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
    cfgs <- IM.withInMemCache (configPilotInMemKey Exophone mocId) 3600 $ CQExo.findAllByMerchantOperatingCityId (Id mocId)
    let configWrappers = map (\cfg -> LYT.Config {config = cfg, extraDimensions = Nothing, identifier = 0}) cfgs
    mapM (\configWrapper -> getConfigImpl a configWrapper (LYT.RIDER_CONFIG Exophone) (Id mocId)) configWrappers
  filterByDimensions dims cfgs = filter matchesDims cfgs
    where
      matchesDims c =
        maybe True (\cs -> c.callService == cs) dims.callService
