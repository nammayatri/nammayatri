{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.ConfigPilot.Config.FinanceRefTypeConfig
  ( FinanceRefTypeConfigDimensions (..),
    fetchRefTypeCatalogue,
  )
where

import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBFlow)
import Kernel.Types.App (MonadFlow)
import Kernel.Types.CacheFlow (CacheFlow)
import Kernel.Types.Id
import qualified Lib.ConfigPilot.Interface.Getter as LCP
import Lib.ConfigPilot.Interface.Types
import qualified Lib.Finance.Domain.Types.FinanceRefTypeConfig as DRC
import qualified Lib.Finance.Storage.Queries.FinanceRefTypeConfig as QRC
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.Beam.Finance ()
import Storage.Beam.Yudhishthira ()

data FinanceRefTypeConfigDimensions = FinanceRefTypeConfigDimensions
  { merchantOperatingCityId :: Text,
    configLogicDomain :: LYT.LogicDomain
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance ConfigTypeInfo 'FinanceRefTypeConfig where
  type DimensionsFor 'FinanceRefTypeConfig = FinanceRefTypeConfigDimensions
  configTypeValue = FinanceRefTypeConfig
  sConfigType = SFinanceRefTypeConfig

instance ConfigDimensions FinanceRefTypeConfigDimensions where
  type ConfigTypeOf FinanceRefTypeConfigDimensions = 'FinanceRefTypeConfig
  type ConfigValueTypeOf FinanceRefTypeConfigDimensions = [DRC.FinanceRefTypeConfig]
  getConfigType _ = FinanceRefTypeConfig
  dimensionsCacheKey a = a.merchantOperatingCityId
  getConfigList a =
    LCP.resolveConfigList
      a
      a.configLogicDomain
      (Id a.merchantOperatingCityId)
      (QRC.findAllByOpCity a.merchantOperatingCityId)
      ([] :: [LCP.DimMatcher FinanceRefTypeConfigDimensions DRC.FinanceRefTypeConfig])
      Nothing

-- | The catalogue for a city, read through ConfigPilot with the plain query as
--   its fallback. Carried onto 'FinanceCtx' so the kernel needs no IO of its own.
fetchRefTypeCatalogue :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> m [DRC.FinanceRefTypeConfig]
fetchRefTypeCatalogue mocId =
  getConfig
    (FinanceRefTypeConfigDimensions mocId (LYT.DRIVER_CONFIG FinanceRefTypeConfig))
    (Just $ QRC.findAllByOpCity mocId)
