{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.ConfigPilot.Config.LeaderBoardConfigs (LeaderBoardConfigsDimensions (..)) where

import qualified Domain.Types.LeaderBoardConfigs as DT
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.ConfigPilot.Interface.Getter as LCP
import Lib.ConfigPilot.Interface.Types
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.Merchant.LeaderBoardConfig as SQ

data LeaderBoardConfigsDimensions = LeaderBoardConfigsDimensions
  { merchantOperatingCityId :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

instance ConfigTypeInfo 'LeaderBoardConfig where
  type DimensionsFor 'LeaderBoardConfig = LeaderBoardConfigsDimensions
  configTypeValue = LeaderBoardConfig
  sConfigType = SLeaderBoardConfig

instance ConfigDimensions LeaderBoardConfigsDimensions where
  type ConfigTypeOf LeaderBoardConfigsDimensions = 'LeaderBoardConfig
  type ConfigValueTypeOf LeaderBoardConfigsDimensions = [DT.LeaderBoardConfigs]
  getConfigType _ = LeaderBoardConfig
  getConfigList a =
    LCP.resolveConfigList
      a
      (LYT.DRIVER_CONFIG LeaderBoardConfig)
      (Id a.merchantOperatingCityId)
      (SQ.findAllByMerchantOpCityId (Id a.merchantOperatingCityId) (Just []))
      ([] :: [LCP.DimMatcher LeaderBoardConfigsDimensions DT.LeaderBoardConfigs])
      Nothing
