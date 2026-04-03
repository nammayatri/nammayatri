{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Types.UI.ReferralPayout where
import EulerHS.Prelude hiding (id)
import Servant
import Tools.Auth
import Data.OpenApi (ToSchema)
import qualified Kernel.Prelude
import qualified Data.Time.Calendar
import qualified Kernel.Types.Common
import qualified Data.Text
import qualified Domain.Types.DailyStats
import qualified Kernel.External.Payment.Juspay.Types.Common
import qualified Domain.Types.PayoutConfig



data DailyEarning
    = DailyEarning {activatedItems :: Kernel.Prelude.Int,
                    earningDate :: Data.Time.Calendar.Day,
                    earnings :: Kernel.Types.Common.HighPrecMoney,
                    payoutOrderId :: Kernel.Prelude.Maybe Data.Text.Text,
                    referrals :: Kernel.Prelude.Int,
                    status :: Domain.Types.DailyStats.PayoutStatus}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)
data ReferralEarningsRes
    = ReferralEarningsRes {d2cReferralCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
                           d2cReferralEarnings :: Kernel.Prelude.Maybe [DailyEarning],
                           d2dReferralCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
                           d2dReferralEarnings :: Kernel.Prelude.Maybe [DailyEarning],
                           dailyEarnings :: [DailyEarning],
                           orderId :: Kernel.Prelude.Maybe Data.Text.Text,
                           orderStatus :: Kernel.Prelude.Maybe Kernel.External.Payment.Juspay.Types.Common.TransactionStatus,
                           payoutRegAmountRefunded :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
                           payoutRegistrationAmount :: Kernel.Types.Common.HighPrecMoney,
                           referralRewardAmountPerRide :: Kernel.Types.Common.HighPrecMoney,
                           referralRewardAmountPerRideForD2DPayout :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
                           totalReferralCount :: Kernel.Prelude.Int,
                           vpaId :: Kernel.Prelude.Maybe Data.Text.Text,
                           vpaVerificationMode :: Domain.Types.PayoutConfig.VpaVerificationMode}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)
data UpdatePayoutVpaReq
    = UpdatePayoutVpaReq {vpa :: Data.Text.Text}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)



