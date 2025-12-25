{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.PopularLocation where

import Data.Aeson
import qualified Data.Text
import qualified Data.Time.Clock
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data PopularLocation = PopularLocation
  { address :: Data.Text.Text,
    createdAt :: Data.Time.Clock.UTCTime,
    id :: Data.Text.Text,
    lat :: Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Double,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    name :: Data.Text.Text,
    rating :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    type_ :: Data.Text.Text,
    updatedAt :: Data.Time.Clock.UTCTime,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant)
  }
  deriving (Generic, Show)
