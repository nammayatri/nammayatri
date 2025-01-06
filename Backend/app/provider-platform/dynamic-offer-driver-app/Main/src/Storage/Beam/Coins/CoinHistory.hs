{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE InstanceSigs #-}

module Storage.Beam.Coins.CoinHistory where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.DriverCoins as DCoins
import qualified Database.Beam as B
import Domain.Types.VehicleCategory as DTV
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import Kernel.Types.Common ()
import Lib.DriverCoins.Types as DCT

data CoinHistoryT f = CoinHistoryT
  { id :: B.C f Text,
    eventFunction :: B.C f DCT.DriverCoinsFunctionType,
    merchantId :: B.C f Text,
    merchantOptCityId :: B.C f Text,
    coins :: B.C f Int,
    driverId :: B.C f Text,
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime,
    expirationAt :: B.C f (Maybe UTCTime),
    status :: B.C f DCT.CoinStatus,
    coinsUsed :: B.C f Int,
    bulkUploadTitle :: B.C f (Maybe DCoins.Translations),
    entityId :: B.C f (Maybe Text),
    vehicleCategory :: B.C f (Maybe DTV.VehicleCategory)
  }
  deriving (Generic, B.Beamable)

instance B.Table CoinHistoryT where
  data PrimaryKey CoinHistoryT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type CoinHistory = CoinHistoryT Identity

$(enableKVPG ''CoinHistoryT ['id] [['driverId]])

$(mkTableInstances ''CoinHistoryT "coin_history" "atlas_driver_offer_bpp")
