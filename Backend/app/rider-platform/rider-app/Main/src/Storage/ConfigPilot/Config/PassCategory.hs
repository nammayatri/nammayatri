{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.ConfigPilot.Config.PassCategory (PassCategoryDimensions (..)) where

import qualified Domain.Types.PassCategory as DT
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.ConfigPilot.Interface.Getter as LCP
import Lib.ConfigPilot.Interface.Types
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.PassCategory as SQ
import qualified Storage.Queries.PassCategory as QPassCategory

data PassCategoryDimensions = PassCategoryDimensions
  { merchantOperatingCityId :: Text,
    configId :: Maybe Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

instance ConfigTypeInfo 'PassCategory where
  type DimensionsFor 'PassCategory = PassCategoryDimensions
  configTypeValue = PassCategory
  sConfigType = SPassCategory

instance ConfigDimensions PassCategoryDimensions where
  type ConfigTypeOf PassCategoryDimensions = 'PassCategory
  type ConfigValueTypeOf PassCategoryDimensions = [DT.PassCategory]
  getConfigType _ = PassCategory
  configDBFetch a =
    Just $ case a.configId of
      Just cId -> maybeToList <$> QPassCategory.findById (Id cId)
      Nothing -> QPassCategory.findAllByMerchantOperatingCityId (Id a.merchantOperatingCityId)
  getConfigList a =
    LCP.resolveConfigList
      a
      (LYT.RIDER_CONFIG PassCategory)
      (Id a.merchantOperatingCityId)
      (SQ.findAllByMerchantOperatingCityId (Id a.merchantOperatingCityId))
      [LCP.DimMatcher (.configId) (\c -> Just c.id.getId) (==)]
      Nothing
