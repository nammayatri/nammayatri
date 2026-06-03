{-# OPTIONS_GHC -Wno-orphans -Wno-deprecations #-}

module Storage.ConfigPilot.Config.FRFSConfig
  ( FRFSConfigDimensions (..),
  )
where

import qualified Domain.Types.FRFSConfig as DFRFS
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.ConfigPilot.Interface.Getter as CR
import Lib.ConfigPilot.Interface.Types
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.FRFSConfig as SCFRFS

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
  type ConfigValueTypeOf FRFSConfigDimensions = Maybe DFRFS.FRFSConfig
  getConfigType _ = FRFSConfig
  getConfigList a =
    listToMaybe
      <$> CR.resolveConfigList
        a
        (LYT.RIDER_CONFIG FRFSConfig)
        (Id a.merchantOperatingCityId)
        (maybeToList <$> SCFRFS.findByMerchantOperatingCityId (Id a.merchantOperatingCityId) (Just []))
        ([] :: [CR.DimMatcher FRFSConfigDimensions DFRFS.FRFSConfig])
        Nothing
