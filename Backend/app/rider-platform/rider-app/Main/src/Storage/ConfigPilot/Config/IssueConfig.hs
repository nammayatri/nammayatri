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

instance ConfigTypeInfo 'IssueConfigRider where
  type DimensionsFor 'IssueConfigRider = IssueConfigDimensions
  configTypeValue = IssueConfigRider
  sConfigType = SIssueConfigRider

instance ConfigDimensions IssueConfigDimensions where
  type ConfigTypeOf IssueConfigDimensions = 'IssueConfigRider
  type ConfigValueTypeOf IssueConfigDimensions = Maybe DT.IssueConfig
  getConfigType _ = IssueConfigRider
  getConfigList a =
    listToMaybe
      <$> LCP.resolveConfigList
        a
        (LYT.RIDER_CONFIG IssueConfig)
        (Id a.merchantOperatingCityId)
        (maybeToList <$> Queries.findByMerchantOpCityId (Id a.merchantOperatingCityId))
        []
        Nothing
