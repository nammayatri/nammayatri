{-# OPTIONS_GHC -Wno-orphans -Wno-deprecations #-}

module Storage.ConfigPilot.Config.RiderConfig
  ( RiderDimensions (..),
  )
where

import qualified Domain.Types.RiderConfig as DRC
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.ConfigPilot.Interface.Getter as CR
import Lib.ConfigPilot.Interface.Types
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC

data RiderDimensions = RiderDimensions
  { merchantOperatingCityId :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

instance ConfigTypeInfo 'RiderConfig where
  type DimensionsFor 'RiderConfig = RiderDimensions
  configTypeValue = RiderConfig
  sConfigType = SRiderConfig

instance ConfigDimensions RiderDimensions where
  type ConfigTypeOf RiderDimensions = 'RiderConfig
  type ConfigValueTypeOf RiderDimensions = Maybe DRC.RiderConfig
  getConfigType _ = RiderConfig
  getConfigList a =
    listToMaybe
      <$> CR.resolveConfigList
        a
        (LYT.RIDER_CONFIG RiderConfig)
        (Id a.merchantOperatingCityId)
        (maybeToList <$> QRC.findByMerchantOperatingCityId (Id a.merchantOperatingCityId))
        ([] :: [CR.DimMatcher RiderDimensions DRC.RiderConfig])
        Nothing
