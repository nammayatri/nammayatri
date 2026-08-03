{-# LANGUAGE DerivingStrategies #-}

module Lib.Finance.Types.AccountRole (AccountRole (..)) where

import Kernel.Prelude

-- | Declarative account roles.
--   Instead of calling 10+ separate getOrCreate*Account functions,
--   just say what role you need and the context fills in the details.
data AccountRole
  = -- Wallet flow accounts
    BuyerAsset
  | BuyerExternal
  | BuyerControl
  | BuyerExpense
  | OwnerLiability
  | OwnerExpense
  | OwnerControl
  | GovtIndirect
  | GovtDirect
  | GovtExpense
  | PlatformAsset
  | PrepaidOwner
  | SellerAsset
  | SellerLiability
  | SellerRideCredit
  | SellerRevenue
  | SellerExpense
  | GovtDirectAsset
  | GovtDirectExpense
  | ParkingFeeRecipient
  | PGPaymentExpense
  | PGPaymentLiability
  | PGPayoutExpense
  | PGPayoutLiability
  | PGGstAsset
  deriving stock (Eq, Show, Generic)
