module External.CCAvenue.Split
  ( calculateSplits,
    isValidSplit,
  )
where

import qualified Data.Text as T
import External.CCAvenue.SplitPayout (SplitLeg (..))
import External.CCAvenue.Types (VendorSplit (..))
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)

totalPercentage :: [VendorSplit] -> Double
totalPercentage = sum . map (\split -> split.percentage)

isValidSplit :: [VendorSplit] -> Bool
isValidSplit splits =
  not (null splits)
    && all (\split -> split.percentage >= 0) splits
    && abs (totalPercentage splits - 100) < 0.001

calculateSplits :: HighPrecMoney -> [VendorSplit] -> Either Text [SplitLeg]
calculateSplits amount splits
  | not (isValidSplit splits) =
    Left $ "invalid vendorSplits: percentages must be non-negative and add up to 100, got " <> show (totalPercentage splits)
  | amount <= 0 = Left $ "amount is not positive: " <> show amount
  | any (\(_, legAmount) -> legAmount <= 0) finalLegs =
    Left "a vendor share rounds to 0.00; the amount is too small for this split config"
  | otherwise = Right $ map toSplitLeg finalLegs
  where
    (initSplits, lastSplits) = splitAt (length splits - 1) splits

    withShare split = (split, roundToPaise $ amount * realToFrac (split.percentage / 100))

    otherLegs = map withShare initSplits

    finalLegs = otherLegs <> map (\split -> (split, amount - sum (map snd otherLegs))) lastSplits

    toSplitLeg (split, legAmount) =
      SplitLeg {splitAmount = showAmount legAmount, subAccId = split.subAccId}

roundToPaise :: HighPrecMoney -> HighPrecMoney
roundToPaise x = fromIntegral (round (x * 100) :: Integer) / 100

showAmount :: HighPrecMoney -> Text
showAmount money =
  let paise = round (money * 100) :: Integer
      (rupees, fraction) = abs paise `divMod` 100
      sign = if paise < 0 then "-" else ""
   in sign <> show rupees <> "." <> T.justifyRight 2 '0' (show fraction)
