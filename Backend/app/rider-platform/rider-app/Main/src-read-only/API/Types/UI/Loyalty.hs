{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.Loyalty where

import Data.OpenApi (ToSchema)
import qualified Data.Time.Calendar
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Lib.Payment.Wallet.Types
import Servant
import Tools.Auth

data DailySpend = DailySpend {amount :: Kernel.Prelude.Int, date :: Data.Time.Calendar.Day}
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LoyaltyInfoResp = LoyaltyInfoResp {programs :: [Lib.Payment.Wallet.Types.LoyaltyProgramSummary]}
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MonthlyExpenseResp = MonthlyExpenseResp {last30DaysSpend :: [DailySpend]}
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
