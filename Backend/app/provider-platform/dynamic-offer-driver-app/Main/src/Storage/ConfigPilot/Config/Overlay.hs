{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.ConfigPilot.Config.Overlay (OverlayDimensions (..)) where

import qualified Domain.Types.Overlay as DT
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.ConfigPilot.Interface.Getter as LCP
import Lib.ConfigPilot.Interface.Types
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.Merchant.Overlay as SQ

data OverlayDimensions = OverlayDimensions
  { merchantOperatingCityId :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

instance ConfigTypeInfo 'Overlay where
  type DimensionsFor 'Overlay = OverlayDimensions
  configTypeValue = Overlay
  sConfigType = SOverlay

instance ConfigDimensions OverlayDimensions where
  type ConfigTypeOf OverlayDimensions = 'Overlay
  type ConfigValueTypeOf OverlayDimensions = [DT.Overlay]
  getConfigType _ = Overlay
  getConfigList a =
    LCP.resolveConfigList
      a
      (LYT.DRIVER_CONFIG Overlay)
      (Id a.merchantOperatingCityId)
      (SQ.findAllByMerchantOpCityId (Id a.merchantOperatingCityId) (Just []))
      ([] :: [LCP.DimMatcher OverlayDimensions DT.Overlay])
      Nothing
