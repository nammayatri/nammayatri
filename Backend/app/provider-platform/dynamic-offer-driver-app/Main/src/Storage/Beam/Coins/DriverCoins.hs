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

module Storage.Beam.Coins.DriverCoins where

import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import Kernel.Types.Common ()
import Lib.DriverCoins.Types as DCT

data DriverCoinsT f = DriverCoinsT
  { id :: B.C f Text,
    fn :: B.C f DCT.DriverCoinsFunctionType,
    eventName :: B.C f Text,
    merchantId :: B.C f Text,
    coins :: B.C f Int,
    expirationAt :: B.C f (Maybe Int),
    trackExpiry :: B.C f Int,
    active :: B.C f Bool
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverCoinsT where
  data PrimaryKey DriverCoinsT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type DriverCoins = DriverCoinsT Identity

$(enableKVPG ''DriverCoinsT ['id] [])

$(mkTableInstances ''DriverCoinsT "coin_config" "atlas_driver_offer_bpp")
