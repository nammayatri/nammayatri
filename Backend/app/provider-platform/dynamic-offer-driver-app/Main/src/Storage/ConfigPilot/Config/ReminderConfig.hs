{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.ConfigPilot.Config.ReminderConfig (ReminderConfigDimensions (..)) where

import qualified Domain.Types.DocumentVerificationConfig
import qualified Domain.Types.ReminderConfig as DT
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.ConfigPilot.Interface.Getter as LCP
import Lib.ConfigPilot.Interface.Types
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.Beam.Yudhishthira ()
import qualified Storage.Queries.ReminderConfig as SQ

data ReminderConfigDimensions = ReminderConfigDimensions
  { merchantOperatingCityId :: Text,
    documentType :: Maybe Domain.Types.DocumentVerificationConfig.DocumentType
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

instance ConfigTypeInfo 'ReminderConfig where
  type DimensionsFor 'ReminderConfig = ReminderConfigDimensions
  configTypeValue = ReminderConfig
  sConfigType = SReminderConfig

instance ConfigDimensions ReminderConfigDimensions where
  type ConfigTypeOf ReminderConfigDimensions = 'ReminderConfig
  type ConfigValueTypeOf ReminderConfigDimensions = [DT.ReminderConfig]
  getConfigType _ = ReminderConfig
  getConfigList a =
    LCP.resolveConfigList
      a
      (LYT.DRIVER_CONFIG ReminderConfig)
      (Id a.merchantOperatingCityId)
      (SQ.findAllByMerchantOpCityId (Id a.merchantOperatingCityId))
      [ LCP.DimMatcher (.documentType) (Just . (.documentType)) (==)
      ]
      Nothing
