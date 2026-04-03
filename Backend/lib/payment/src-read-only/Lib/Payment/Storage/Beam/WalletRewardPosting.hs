{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Lib.Payment.Storage.Beam.WalletRewardPosting where
import Kernel.Prelude
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Lib.Payment.Storage.Beam.BeamFlow ()
import qualified Kernel.Types.Common
import qualified Kernel.Prelude
import qualified Lib.Payment.Domain.Types.WalletRewardPosting
import qualified Kernel.External.Wallet.Interface
import qualified Database.Beam as B



data WalletRewardPostingT f
    = WalletRewardPostingT {cashAmount :: (B.C f Kernel.Types.Common.HighPrecMoney),
                            createdAt :: (B.C f Kernel.Prelude.UTCTime),
                            id :: (B.C f Kernel.Prelude.Text),
                            merchantId :: (B.C f Kernel.Prelude.Text),
                            merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                            pointsAmount :: (B.C f Kernel.Types.Common.HighPrecMoney),
                            postingType :: (B.C f Kernel.External.Wallet.Interface.WalletPostingType),
                            shortId :: (B.C f Kernel.Prelude.Text),
                            status :: (B.C f Lib.Payment.Domain.Types.WalletRewardPosting.WalletPostingStatus),
                            updatedAt :: (B.C f Kernel.Prelude.UTCTime),
                            walletId :: (B.C f Kernel.Prelude.Text)}
    deriving (Generic, B.Beamable)
instance B.Table WalletRewardPostingT
    where data PrimaryKey WalletRewardPostingT f = WalletRewardPostingId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = WalletRewardPostingId . id
type WalletRewardPosting = WalletRewardPostingT Identity

$(enableKVPG (''WalletRewardPostingT) [('id)] [])

$(mkTableInstancesGenericSchema (''WalletRewardPostingT) "wallet_reward_posting")

