{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.ShuttlePublishedTrip where

import Data.Aeson
import qualified Data.Time
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data ShuttlePublishedTrip = ShuttlePublishedTrip
  { createdAt :: Data.Time.UTCTime,
    day :: Kernel.Prelude.Text,
    departure :: Kernel.Prelude.Text,
    feedKey :: Kernel.Prelude.Text,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    riders :: Kernel.Prelude.Int,
    routeCode :: Kernel.Prelude.Text,
    tripId :: Kernel.Prelude.Text,
    updatedAt :: Data.Time.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
