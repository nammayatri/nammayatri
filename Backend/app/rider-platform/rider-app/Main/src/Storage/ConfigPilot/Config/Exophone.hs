{-# OPTIONS_GHC -Wno-orphans -Wno-deprecations #-}

module Storage.ConfigPilot.Config.Exophone
  ( ExophoneDimensions (..),
  )
where

import qualified Domain.Types.Exophone as DE
import Kernel.External.Call.Types (CallService)
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.ConfigPilot.Interface.Getter as CR
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
  getConfigList a =
    CR.resolveConfigList
      a
      (LYT.RIDER_CONFIG Exophone)
      (Id a.merchantOperatingCityId)
      (CQExo.findAllByMerchantOperatingCityId (Id a.merchantOperatingCityId))
      [CR.DimMatcher (.callService) (Just . (.callService)) (==)]
      Nothing
