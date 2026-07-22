{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.ConfigPilot.Config.FRFSConfig (FRFSConfigDimensions (..)) where

import qualified Domain.Types.FRFSConfig as DT
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.ConfigPilot.Interface.Getter as LCP
import Lib.ConfigPilot.Interface.Types
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.FRFSConfig as SQ

data FRFSConfigDimensions = FRFSConfigDimensions
  { merchantOperatingCityId :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

instance ConfigTypeInfo 'FRFSConfig where
  type DimensionsFor 'FRFSConfig = FRFSConfigDimensions
  configTypeValue = FRFSConfig
  sConfigType = SFRFSConfig

instance ConfigDimensions FRFSConfigDimensions where
  type ConfigTypeOf FRFSConfigDimensions = 'FRFSConfig
  type ConfigValueTypeOf FRFSConfigDimensions = Maybe DT.FRFSConfig
  getConfigType _ = FRFSConfig
  getConfigList a =
    listToMaybe
      <$> LCP.resolveConfigList
        a
        (LYT.RIDER_CONFIG FRFSConfig)
        (Id a.merchantOperatingCityId)
        (maybeToList <$> SQ.findByMerchantOperatingCityId (Id a.merchantOperatingCityId) (Just []))
        (([] :: [LCP.DimMatcher FRFSConfigDimensions DT.FRFSConfig]))
        Nothing
  configFallback a = Just (SQ.findByMerchantOperatingCityId (Id a.merchantOperatingCityId) (Just []))
