{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.ConfigPilot.Config.IssueConfig (IssueConfigDimensions (..)) where

import qualified IssueManagement.Domain.Types.Issue.IssueConfig as DT
import qualified IssueManagement.Storage.Queries.Issue.IssueConfig as Queries
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.ConfigPilot.Interface.Getter as LCP
import Lib.ConfigPilot.Interface.Types
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.Beam.IssueManagement ()
import Storage.Beam.Yudhishthira ()

data IssueConfigDimensions = IssueConfigDimensions
  { merchantOperatingCityId :: Text,
    identifier :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

instance ConfigTypeInfo 'IssueConfigDriver where
  type DimensionsFor 'IssueConfigDriver = IssueConfigDimensions
  configTypeValue = IssueConfigDriver
  sConfigType = SIssueConfigDriver

instance ConfigDimensions IssueConfigDimensions where
  type ConfigTypeOf IssueConfigDimensions = 'IssueConfigDriver
  type ConfigValueTypeOf IssueConfigDimensions = Maybe DT.IssueConfig
  getConfigType _ = IssueConfigDriver
  getConfigList a =
    listToMaybe
      <$> LCP.resolveConfigList
        a
        (LYT.DRIVER_CONFIG IssueConfig)
        (Id a.merchantOperatingCityId)
        (maybeToList <$> Queries.findByMerchantOpCityId (Id a.merchantOperatingCityId))
        []
        Nothing
