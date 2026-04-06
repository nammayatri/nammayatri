{-# OPTIONS_GHC -Wno-orphans -Wno-deprecations #-}

module Storage.ConfigPilot.Config.RiderConfig
  ( RiderDimensions (..),
  )
where

import qualified Domain.Types.RiderConfig as DRC
import Kernel.Prelude
import qualified Kernel.Storage.InMem as IM
import Kernel.Types.Id
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import Storage.ConfigPilot.Interface.Getter
import Storage.ConfigPilot.Interface.Types

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
  getConfigList a = do
    let mocId = a.merchantOperatingCityId
    IM.withInMemCache (configPilotInMemKey a) 3600 $ do
      cfg <- QRC.findByMerchantOperatingCityId (Id mocId)
      case cfg of
        Nothing -> pure Nothing
        Just c -> do
          let configWrapper = LYT.Config {config = c, extraDimensions = Nothing, identifier = 0}
          Just <$> getConfigImpl a configWrapper (LYT.RIDER_CONFIG RiderConfig) (Id mocId)
