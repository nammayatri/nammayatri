{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.Coins.PurchaseHistory where

import qualified Database.Beam as B
import Database.Beam.MySQL ()
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common (HighPrecMoney)

data PurchaseHistoryT f = PurchaseHistoryT
  { id :: B.C f Text,
    numCoins :: B.C f Int,
    merchantId :: B.C f Text,
    merchantOptCityId :: B.C f Text,
    driverId :: B.C f Text,
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime,
    title :: B.C f Text,
    cash :: B.C f HighPrecMoney -- money converted that can be used
  }
  deriving (Generic, B.Beamable)

instance B.Table PurchaseHistoryT where
  data PrimaryKey PurchaseHistoryT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type PurchaseHistory = PurchaseHistoryT Identity

$(enableKVPG ''PurchaseHistoryT ['id] [])

$(mkTableInstances ''PurchaseHistoryT "coin_purchase_history" "atlas_driver_offer_bpp")
