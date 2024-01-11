{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.MerchantOperatingCity where

import qualified Domain.Types.Merchant
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id

data MerchantOperatingCity = MerchantOperatingCity
  { city :: Kernel.Types.Beckn.Context.City,
    id :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantShortId :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
