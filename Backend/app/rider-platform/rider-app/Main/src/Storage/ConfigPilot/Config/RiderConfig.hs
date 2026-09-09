{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.ConfigPilot.Config.RiderConfig (RiderConfigDimensions (..)) where

import qualified Domain.Types.RiderConfig as DT
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.ConfigPilot.Interface.Getter as LCP
import Lib.ConfigPilot.Interface.Types
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.Merchant.RiderConfig as SQ

data RiderConfigDimensions = RiderConfigDimensions
  { merchantOperatingCityId :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

instance ConfigTypeInfo 'RiderConfig where
  type DimensionsFor 'RiderConfig = RiderConfigDimensions
  configTypeValue = RiderConfig
  sConfigType = SRiderConfig

instance ConfigDimensions RiderConfigDimensions where
  type ConfigTypeOf RiderConfigDimensions = 'RiderConfig
  type ConfigValueTypeOf RiderConfigDimensions = Maybe DT.RiderConfig
  getConfigType _ = RiderConfig
  getConfigList a =
    listToMaybe
      <$> LCP.resolveConfigList
        a
        (LYT.RIDER_CONFIG RiderConfig)
        (Id a.merchantOperatingCityId)
        (maybeToList <$> SQ.findByMerchantOperatingCityId (Id a.merchantOperatingCityId))
        (([] :: [LCP.DimMatcher RiderConfigDimensions DT.RiderConfig]))
        Nothing
