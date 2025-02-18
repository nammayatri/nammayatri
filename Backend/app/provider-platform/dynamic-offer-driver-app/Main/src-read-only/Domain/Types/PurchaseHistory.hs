{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.PurchaseHistory where

import Data.Aeson
import qualified Domain.Types.VehicleCategory
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data PurchaseHistory = PurchaseHistory
  { cash :: Kernel.Types.Common.HighPrecMoney,
    createdAt :: Kernel.Prelude.UTCTime,
    currency :: Kernel.Types.Common.Currency,
    driverId :: Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.PurchaseHistory.PurchaseHistory,
    merchantId :: Kernel.Prelude.Text,
    merchantOptCityId :: Kernel.Prelude.Text,
    numCoins :: Kernel.Prelude.Int,
    title :: Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime,
    vehicleCategory :: Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
