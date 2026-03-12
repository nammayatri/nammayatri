{-# LANGUAGE InstanceSigs #-}

module Storage.Beam.MonetaryReward.MonetaryRewardConfig where

import qualified Database.Beam as B
import Domain.Types.VehicleCategory as DTV
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import Kernel.Types.Common ()
import qualified Kernel.Types.Common as KTC
import Lib.DriverCoins.Types as DCT

data MonetaryRewardConfigT f = MonetaryRewardConfigT
  { id :: B.C f Text,
    eventFunction :: B.C f DCT.DriverCoinsFunctionType,
    eventName :: B.C f Text,
    merchantId :: B.C f Text,
    merchantOptCityId :: B.C f Text,
    monetaryRewardAmount :: B.C f KTC.HighPrecMoney,
    expirationAt :: B.C f (Maybe Int),
    active :: B.C f Bool,
    vehicleCategory :: B.C f (Maybe DTV.VehicleCategory)
  }
  deriving (Generic, B.Beamable)

instance B.Table MonetaryRewardConfigT where
  data PrimaryKey MonetaryRewardConfigT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type MonetaryRewardConfig = MonetaryRewardConfigT Identity

$(enableKVPG ''MonetaryRewardConfigT ['id] [['merchantId]])

$(mkTableInstances ''MonetaryRewardConfigT "monetary_reward_config" "atlas_driver_offer_bpp")
