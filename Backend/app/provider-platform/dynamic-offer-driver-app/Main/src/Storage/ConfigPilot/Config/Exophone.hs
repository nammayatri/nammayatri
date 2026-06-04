{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.ConfigPilot.Config.Exophone (ExophoneDimensions (..)) where

import qualified Domain.Types.Exophone as DT
import qualified Kernel.External.Call.Types
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.ConfigPilot.Interface.Getter as LCP
import Lib.ConfigPilot.Interface.Types
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.Exophone as SQ

data ExophoneDimensions = ExophoneDimensions
  { merchantOperatingCityId :: Text,
    callService :: Maybe Kernel.External.Call.Types.CallService
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

instance ConfigTypeInfo 'Exophone where
  type DimensionsFor 'Exophone = ExophoneDimensions
  configTypeValue = Exophone
  sConfigType = SExophone

instance ConfigDimensions ExophoneDimensions where
  type ConfigTypeOf ExophoneDimensions = 'Exophone
  type ConfigValueTypeOf ExophoneDimensions = [DT.Exophone]
  getConfigType _ = Exophone
  getConfigList a =
    LCP.resolveConfigList
      a
      (LYT.DRIVER_CONFIG Exophone)
      (Id a.merchantOperatingCityId)
      (SQ.findAllByMerchantOpCityId (Id a.merchantOperatingCityId))
      [ LCP.DimMatcher (.callService) (Just . (.callService)) (==)
      ]
      Nothing
