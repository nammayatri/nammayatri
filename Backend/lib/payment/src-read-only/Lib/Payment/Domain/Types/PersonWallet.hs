{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Domain.Types.PersonWallet where

import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id

data PersonWallet = PersonWallet
  { cashAmount :: Kernel.Types.Common.HighPrecMoney,
    cashFromPointsRedemption :: Kernel.Types.Common.HighPrecMoney,
    createdAt :: Kernel.Prelude.UTCTime,
    expiredBalance :: Kernel.Types.Common.HighPrecMoney,
    id :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.PersonWallet.PersonWallet,
    merchantId :: Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    personId :: Kernel.Prelude.Text,
    pointsAmount :: Kernel.Types.Common.HighPrecMoney,
    updatedAt :: Kernel.Prelude.UTCTime,
    usableCashAmount :: Kernel.Types.Common.HighPrecMoney,
    usablePointsAmount :: Kernel.Types.Common.HighPrecMoney
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
