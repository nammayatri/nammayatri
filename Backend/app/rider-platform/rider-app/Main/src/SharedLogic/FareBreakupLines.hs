module SharedLogic.FareBreakupLines
  ( FareLines (..),
    FareDiscount (..),
    fareLinesFromPairs,
    fareLinesFromBreakups,
    offerBasePrice,
    applyOffer,
    fareTaxesFromBreakup,
  )
where

import qualified BecknV2.OnDemand.FareBreakup.Core as FB
import BecknV2.OnDemand.FareBreakup.Tax (FareTaxes)
import qualified BecknV2.OnDemand.FareBreakup.Tax as Tax
import qualified Data.Text as T
import qualified Domain.Types.FareBreakup as DFareBreakup
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)

data FareLines = FareLines
  { fare :: HighPrecMoney,
    breakupLines :: [FB.BreakupLine]
  }
  deriving (Show)

data FareDiscount = FareDiscount
  { appliedDiscount :: HighPrecMoney,
    totalBefore :: HighPrecMoney,
    totalAfter :: HighPrecMoney
  }
  deriving (Show, Generic)

fareLinesFromPairs :: HighPrecMoney -> [(Text, HighPrecMoney)] -> Maybe FareLines
fareLinesFromPairs fareAmount pairs
  | null pairs = Nothing
  | not (null rateCardLines) = Just (FareLines fareAmount rateCardLines)
  | otherwise = Just (FareLines fareAmount (map (uncurry FB.BreakupLine) pairs))
  where
    rateCardLines = [FB.BreakupLine title amount | (t, amount) <- pairs, Just title <- [T.stripSuffix "_FARE_PARAM" t]]

fareLinesFromBreakups :: HighPrecMoney -> [DFareBreakup.FareBreakup] -> Maybe FareLines
fareLinesFromBreakups fareAmount = fareLinesFromPairs fareAmount . map (\fb -> (fb.description, fb.amount.amount))

offerBasePrice :: FareLines -> HighPrecMoney
offerBasePrice fl = FB.discountableAmount fl.fare fl.breakupLines

applyOffer :: HighPrecMoney -> FareLines -> FareDiscount
applyOffer discount fl =
  let applied = max 0 (min discount (offerBasePrice fl))
   in FareDiscount
        { appliedDiscount = applied,
          totalBefore = fl.fare,
          totalAfter = fl.fare - applied
        }

fareTaxesFromBreakup :: [FB.BreakupLine] -> FareTaxes
fareTaxesFromBreakup ls = componentTax <$> FB.taxTags <*> FB.taxAmountTitles
  where
    nonZero = mfilter (/= 0) . Just
    amountOf tag = nonZero (FB.linesAmount (== FB.tagTitle tag) ls)
    componentTax tags amountTitles =
      (\taxAmount -> Tax.TaxBreakup {total = taxAmount, cgst = amountOf tags.cgst, sgst = amountOf tags.sgst, igst = amountOf tags.igst})
        <$> nonZero (FB.linesAmount (`elem` amountTitles) ls)
