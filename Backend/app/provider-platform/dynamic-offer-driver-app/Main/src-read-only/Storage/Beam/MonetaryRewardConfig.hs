{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.MonetaryRewardConfig where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Lib.DriverCoins.Types
import qualified Kernel.Types.Common
import qualified Domain.Types.VehicleCategory
import qualified Database.Beam as B



data MonetaryRewardConfigT f
    = MonetaryRewardConfigT {active :: (B.C f Kernel.Prelude.Bool),
                             eventFunction :: (B.C f Lib.DriverCoins.Types.DriverCoinsFunctionType),
                             eventName :: (B.C f Kernel.Prelude.Text),
                             expirationAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                             id :: (B.C f Kernel.Prelude.Text),
                             merchantId :: (B.C f Kernel.Prelude.Text),
                             merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
                             monetaryRewardAmount :: (B.C f Kernel.Types.Common.HighPrecMoney),
                             vehicleCategory :: (B.C f (Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory)),
                             createdAt :: (B.C f Kernel.Prelude.UTCTime),
                             updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table MonetaryRewardConfigT
    where data PrimaryKey MonetaryRewardConfigT f = MonetaryRewardConfigId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = MonetaryRewardConfigId . id
type MonetaryRewardConfig = MonetaryRewardConfigT Identity

$(enableKVPG (''MonetaryRewardConfigT) [('id)] [])

$(mkTableInstances (''MonetaryRewardConfigT) "monetary_reward_config")

