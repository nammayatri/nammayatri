{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.ConfigPilot.Config.MerchantMessage (MerchantMessageDimensions (..)) where

import qualified Domain.Types.MerchantMessage as DT
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.ConfigPilot.Interface.Getter as LCP
import Lib.ConfigPilot.Interface.Types
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.Merchant.MerchantMessage as SQ

data MerchantMessageDimensions = MerchantMessageDimensions
  { merchantOperatingCityId :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

instance ConfigTypeInfo 'MerchantMessage where
  type DimensionsFor 'MerchantMessage = MerchantMessageDimensions
  configTypeValue = MerchantMessage
  sConfigType = SMerchantMessage

instance ConfigDimensions MerchantMessageDimensions where
  type ConfigTypeOf MerchantMessageDimensions = 'MerchantMessage
  type ConfigValueTypeOf MerchantMessageDimensions = [DT.MerchantMessage]
  getConfigType _ = MerchantMessage
  getConfigList a =
    LCP.resolveConfigList
      a
      (LYT.DRIVER_CONFIG MerchantMessage)
      (Id a.merchantOperatingCityId)
      (SQ.findAllByMerchantOpCityId (Id a.merchantOperatingCityId) (Just []))
      ([] :: [LCP.DimMatcher MerchantMessageDimensions DT.MerchantMessage])
      Nothing
