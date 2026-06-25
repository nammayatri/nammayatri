{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.ConfigPilot.Config.CancellationReason (CancellationReasonDimensions (..)) where

import qualified Domain.Types.CancellationReason as DT
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.ConfigPilot.Interface.Getter as LCP
import Lib.ConfigPilot.Interface.Types
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.Beam.Yudhishthira ()
import qualified Storage.Queries.CancellationReason as SQ

data CancellationReasonDimensions = CancellationReasonDimensions
  { merchantOperatingCityId :: Text,
    cancellationStage :: Maybe DT.CancellationStage
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

instance ConfigTypeInfo 'CancellationReason where
  type DimensionsFor 'CancellationReason = CancellationReasonDimensions
  configTypeValue = CancellationReason
  sConfigType = SCancellationReason

instance ConfigDimensions CancellationReasonDimensions where
  type ConfigTypeOf CancellationReasonDimensions = 'CancellationReason
  type ConfigValueTypeOf CancellationReasonDimensions = [DT.CancellationReason]
  getConfigType _ = CancellationReason
  getConfigList a =
    LCP.resolveConfigList
      a
      (LYT.RIDER_CONFIG CancellationReason)
      (Id a.merchantOperatingCityId)
      (maybe (pure []) SQ.findAll a.cancellationStage)
      (([] :: [LCP.DimMatcher CancellationReasonDimensions DT.CancellationReason]))
      Nothing
