module Domain.Types.FinanceRefType
  ( FinanceRefType (..),
    financeRefToText,
    financeRefFromText,
    module Domain.SharedLogic.RideDiscount,
  )
where

import Domain.SharedLogic.RideDiscount (RideFinanceRefType (..), rideFinanceRefFromText, rideFinanceRefToText)
import Kernel.Prelude

-- | Finance reference types specific to this app. Ride-lifecycle ref types are
--   shared and re-exported from 'Domain.SharedLogic.RideDiscount'.
data FinanceRefType
  = AirportCashRecharge
  | AirportCashWithdrawal
  | AirportEntryFee
  | AirportEntryFeeGST
  | D2DReferral
  | DriverCancellationCharges
  | ExpiryCreditTransfer
  | ExpiryRevenueRecognition
  | RideRevenueRecognition
  | RideSubscriptionDebit
  | SubscriptionCredit
  | SubscriptionPurchase
  | TDSReimbursement
  | WalletIncentive
  | WalletPayout
  | WalletTopup
  deriving (Eq, Ord, Show, Read, Generic, Bounded, Enum, ToJSON, FromJSON, ToSchema)

financeRefToText :: FinanceRefType -> Text
financeRefToText = show

financeRefFromText :: Text -> Maybe FinanceRefType
financeRefFromText = readMaybe . toString
