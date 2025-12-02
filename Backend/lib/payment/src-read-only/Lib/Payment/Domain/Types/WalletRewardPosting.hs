{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Domain.Types.WalletRewardPosting where

import qualified Kernel.Beam.Lib.UtilsTH
import qualified Kernel.External.Wallet.Interface
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Lib.Payment.Domain.Types.PersonWallet

data WalletRewardPosting = WalletRewardPosting
  { cashAmount :: Kernel.Types.Common.HighPrecMoney,
    createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.WalletRewardPosting.WalletRewardPosting,
    merchantId :: Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    pointsAmount :: Kernel.Types.Common.HighPrecMoney,
    postingType :: Kernel.External.Wallet.Interface.WalletPostingType,
    shortId :: Kernel.Types.Id.ShortId Lib.Payment.Domain.Types.WalletRewardPosting.WalletRewardPosting,
    status :: Lib.Payment.Domain.Types.WalletRewardPosting.WalletPostingStatus,
    updatedAt :: Kernel.Prelude.UTCTime,
    walletId :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.PersonWallet.PersonWallet
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data WalletPostingStatus = NEW | SUCCESS | FAILED deriving (Show, (Eq), (Ord), (Read), (Generic), (ToJSON), (FromJSON), (ToSchema))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum (''Lib.Payment.Domain.Types.WalletRewardPosting.WalletPostingStatus))
