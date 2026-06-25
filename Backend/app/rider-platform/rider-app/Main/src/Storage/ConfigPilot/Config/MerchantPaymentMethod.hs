{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.ConfigPilot.Config.MerchantPaymentMethod (MerchantPaymentMethodDimensions (..)) where

import qualified Domain.Types.MerchantPaymentMethod as DT
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.ConfigPilot.Interface.Getter as LCP
import Lib.ConfigPilot.Interface.Types
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.Merchant.MerchantPaymentMethod as SQ

data MerchantPaymentMethodDimensions = MerchantPaymentMethodDimensions
  { merchantOperatingCityId :: Text,
    configId :: Maybe Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

instance ConfigTypeInfo 'MerchantPaymentMethod where
  type DimensionsFor 'MerchantPaymentMethod = MerchantPaymentMethodDimensions
  configTypeValue = MerchantPaymentMethod
  sConfigType = SMerchantPaymentMethod

instance ConfigDimensions MerchantPaymentMethodDimensions where
  type ConfigTypeOf MerchantPaymentMethodDimensions = 'MerchantPaymentMethod
  type ConfigValueTypeOf MerchantPaymentMethodDimensions = [DT.MerchantPaymentMethod]
  getConfigType _ = MerchantPaymentMethod
  getConfigList a =
    let fetch = case a.configId of
          Just cId -> maybeToList <$> SQ.findById (Id cId)
          Nothing -> SQ.findAllByMerchantOperatingCityId (Id a.merchantOperatingCityId)
     in LCP.resolveConfigList
          a
          (LYT.RIDER_CONFIG MerchantPaymentMethod)
          (Id a.merchantOperatingCityId)
          fetch
          [LCP.DimMatcher (.configId) (\c -> Just c.id.getId) (==)]
          Nothing
