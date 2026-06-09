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

instance ConfigTypeInfo 'ExophoneRider where
  type DimensionsFor 'ExophoneRider = ExophoneDimensions
  configTypeValue = ExophoneRider
  sConfigType = SExophoneRider

instance ConfigDimensions ExophoneDimensions where
  type ConfigTypeOf ExophoneDimensions = 'ExophoneRider
  type ConfigValueTypeOf ExophoneDimensions = [DT.Exophone]
  getConfigType _ = ExophoneRider
  getConfigList a =
    LCP.resolveConfigList
      a
      (LYT.RIDER_CONFIG ExophoneRider)
      (Id a.merchantOperatingCityId)
      (SQ.findAllByMerchantOperatingCityId (Id a.merchantOperatingCityId))
      [ LCP.DimMatcher (.callService) (Just . (.callService)) (==)
      ]
      Nothing
