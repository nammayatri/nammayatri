{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.PurchaseHistory where

import Data.Aeson
import qualified Domain.Types.VehicleCategory
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Utils.TH
import qualified Lib.Payment.Domain.Types.PayoutOrder
import qualified Tools.Beam.UtilsTH

data PurchaseHistory = PurchaseHistory
  { cash :: Kernel.Types.Common.HighPrecMoney,
    coinRedemptionType :: Domain.Types.PurchaseHistory.CoinRedemptionType,
    createdAt :: Kernel.Prelude.UTCTime,
    currency :: Kernel.Types.Common.Currency,
    driverId :: Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.PurchaseHistory.PurchaseHistory,
    merchantId :: Kernel.Prelude.Text,
    merchantOptCityId :: Kernel.Prelude.Text,
    numCoins :: Kernel.Prelude.Int,
    payoutOrderIdForDirectPayout :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutOrder.PayoutOrder),
    title :: Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime,
    vehicleCategory :: Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data CoinRedemptionType = SubscriptionUse | DirectPayout deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Ord, Eq, Read, ToParamSchema)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList ''CoinRedemptionType)

$(Kernel.Utils.TH.mkHttpInstancesForEnum ''CoinRedemptionType)
