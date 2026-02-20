{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.PurchaseHistory where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.PurchaseHistory
import qualified Domain.Types.VehicleCategory
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data PurchaseHistoryT f = PurchaseHistoryT
  { cash :: B.C f Kernel.Types.Common.HighPrecMoney,
    coinRedemptionType :: B.C f (Kernel.Prelude.Maybe Domain.Types.PurchaseHistory.CoinRedemptionType),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    currency :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency),
    driverId :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOptCityId :: B.C f Kernel.Prelude.Text,
    numCoins :: B.C f Kernel.Prelude.Int,
    payoutOrderIdForDirectPayout :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    title :: B.C f Kernel.Prelude.Text,
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    vehicleCategory :: B.C f (Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory)
  }
  deriving (Generic, B.Beamable)

instance B.Table PurchaseHistoryT where
  data PrimaryKey PurchaseHistoryT f = PurchaseHistoryId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = PurchaseHistoryId . id

type PurchaseHistory = PurchaseHistoryT Identity

$(enableKVPG ''PurchaseHistoryT ['id] [['driverId]])

$(Kernel.Beam.Lib.UtilsTH.mkTableInstances ''PurchaseHistoryT "coin_purchase_history" "atlas_driver_offer_bpp")
