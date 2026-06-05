{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.ConfigPilot.Config.FleetOwnerDocumentVerificationConfig (FleetOwnerDocumentVerificationConfigDimensions (..)) where

import qualified Domain.Types.FleetOwnerDocumentVerificationConfig as DT
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.ConfigPilot.Interface.Getter as LCP
import Lib.ConfigPilot.Interface.Types
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.FleetOwnerDocumentVerificationConfig as SQ

data FleetOwnerDocumentVerificationConfigDimensions = FleetOwnerDocumentVerificationConfigDimensions
  { merchantOperatingCityId :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

instance ConfigTypeInfo 'FleetOwnerDocumentVerificationConfig where
  type DimensionsFor 'FleetOwnerDocumentVerificationConfig = FleetOwnerDocumentVerificationConfigDimensions
  configTypeValue = FleetOwnerDocumentVerificationConfig
  sConfigType = SFleetOwnerDocumentVerificationConfig

instance ConfigDimensions FleetOwnerDocumentVerificationConfigDimensions where
  type ConfigTypeOf FleetOwnerDocumentVerificationConfigDimensions = 'FleetOwnerDocumentVerificationConfig
  type ConfigValueTypeOf FleetOwnerDocumentVerificationConfigDimensions = [DT.FleetOwnerDocumentVerificationConfig]
  getConfigType _ = FleetOwnerDocumentVerificationConfig
  getConfigList a =
    LCP.resolveConfigList
      a
      (LYT.DRIVER_CONFIG FleetOwnerDocumentVerificationConfig)
      (Id a.merchantOperatingCityId)
      (SQ.findAllByMerchantOpCityId (Id a.merchantOperatingCityId) (Just []))
      ([] :: [LCP.DimMatcher FleetOwnerDocumentVerificationConfigDimensions DT.FleetOwnerDocumentVerificationConfig])
      Nothing
