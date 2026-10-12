module RideFinance.Display.Present
  ( Line (..),
    LineRole (..),
    EarningsLayout (..),
    defaultKeys,
    presentCustomer,
    presentEarnings,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)
import RideFinance.Core.Tax (inclusive)
import RideFinance.Core.Types
import RideFinance.Display.Club (Keys (..))

data LineRole = Addition | Deduction | Memo
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

data Line = Line
  { code :: Text,
    part :: Maybe AmountPart,
    component :: Maybe FareComponent,
    role :: LineRole,
    amount :: HighPrecMoney
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data EarningsLayout = EarningsLayout
  { componentKeys :: FareComponent -> Keys Text,
    customerPaidCode :: Text,
    discountCode :: Text,
    tipCode :: Text,
    paymentChargeCollectedCode :: Text,
    memoCode :: MemoItem -> Text
  }

defaultKeys :: FareComponent -> Keys Text
defaultKeys c = Keys {taxExclusiveKeys = [show c], taxKeys = [show c <> "_TAX"], clubbedKey = Just (show c <> "_INCL_TAX")}

componentLines :: (FareComponent -> Keys Text) -> ClubPolicy -> LineRole -> (FareComponent, [TaxedAmount]) -> [Line]
componentLines keysOf policy lineRole (c, parts)
  | excl == 0 && vat == 0 = []
  | vat == 0 = line TaxExclusivePart keys.taxExclusiveKeys excl
  | Set.member c policy = line TaxInclusivePart (maybeToList keys.clubbedKey) (excl + vat)
  | otherwise = line TaxExclusivePart keys.taxExclusiveKeys excl <> line TaxPart keys.taxKeys vat
  where
    keys = keysOf c
    excl = sum (map (.taxExclusive) parts)
    vat = sum (map (.tax) parts)
    sign = if lineRole == Deduction then -1 else 1
    line amountPart candidates value =
      [Line {code = lineCode, part = Just amountPart, component = Just c, role = lineRole, amount = sign * value} | lineCode <- take 1 candidates]

flatLine :: LineRole -> Text -> HighPrecMoney -> Line
flatLine lineRole lineCode value = Line {code = lineCode, part = Nothing, component = Nothing, role = lineRole, amount = value}

presentCustomer :: (FareComponent -> Keys Text) -> ClubPolicy -> RideFinance -> [Line]
presentCustomer keysOf policy rf =
  concatMap (componentLines keysOf policy Addition) (Map.toList rf.charges)
    <> [flatLine Deduction "DISCOUNT" (negate rf.appliedDiscount) | rf.appliedDiscount /= 0]
    <> [flatLine Addition "TIP" rf.tip | rf.tip /= 0]

presentEarnings :: EarningsLayout -> ClubPolicy -> RideFinance -> [Line]
presentEarnings layout policy rf =
  [flatLine Addition layout.customerPaidCode customerPaid]
    <> separatedTax
    <> [flatLine Addition layout.discountCode rf.appliedDiscount | rf.appliedDiscount /= 0]
    <> [flatLine Memo layout.tipCode rf.tip | rf.tip /= 0]
    <> concatMap (componentLines keysOf policy Deduction . fmap pure) otherDeductions
    <> paymentChargeLines
    <> [flatLine Memo (layout.memoCode item) value | (item, value) <- Map.toList rf.memos]
  where
    keysOf = layout.componentKeys
    earnedCharges = filter (earnedByDriver . fst) (Map.toList rf.charges)
    separatedTax = filter ((== Just TaxPart) . (.part)) (concatMap (componentLines keysOf policy Addition) earnedCharges)
    customerPaid = sum (map inclusive (concatMap snd earnedCharges)) - rf.appliedDiscount + rf.tip - sum (map (.amount) separatedTax)
    otherDeductions = filter ((/= PAYMENT_CHARGE) . fst) (Map.toList rf.deductions)
    collectedCharge = inclusive rf.paymentCharge
    chargeDeduction = componentLines keysOf policy Deduction (PAYMENT_CHARGE, [rf.paymentCharge])
    paymentChargeLines = case rf.chargeBearer of
      Just PAYMENT_CUSTOMER -> [flatLine Addition layout.paymentChargeCollectedCode collectedCharge | collectedCharge /= 0] <> chargeDeduction
      Just PAYMENT_DRIVER -> chargeDeduction
      _ -> []
