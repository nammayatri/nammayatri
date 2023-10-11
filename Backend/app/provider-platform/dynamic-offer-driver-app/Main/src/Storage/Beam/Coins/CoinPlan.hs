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

module Storage.Beam.Coins.CoinPlan where

import qualified Database.Beam as B
import Database.Beam.MySQL ()
import qualified Domain.Types.Plan as DPlan
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)

data CoinPlanT f = CoinPlanT
  { id :: B.C f Text,
    subPlanId :: B.C f Text,
    subPlanMode :: B.C f DPlan.PaymentMode,
    requiredCoins :: B.C f Int,
    numOfDays :: B.C f Int,
    coinPlanName :: B.C f Text,
    merchantId :: B.C f Text
  }
  deriving (Generic, B.Beamable)

instance B.Table CoinPlanT where
  data PrimaryKey CoinPlanT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type CoinPlan = CoinPlanT Identity

$(enableKVPG ''CoinPlanT ['id] [])

$(mkTableInstances ''CoinPlanT "coin_plan" "atlas_driver_offer_bpp")
