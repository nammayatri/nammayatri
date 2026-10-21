-- | Driver earnings fare breakup: turns the city's 'FareBreakupNode' config and
-- a ride's 'FareParameters' into a tree of display lines.
--
-- A leaf shows the sum of the fare components it lists in 'fareTags' -- the
-- same 'FareChargeComponent's fare policy charge configs use, read from
-- 'buildComponentMap'; a group shows the sum of its children. Amounts are
-- rounded to the currency's accuracy with the largest-remainder method, so
-- every level adds up exactly to the line above it.
module SharedLogic.FareBreakup
  ( FareBreakupLine (..),
    breakupComponentAmounts,
    validateFareBreakupConfig,
    computeFareBreakup,
  )
where

import Data.List (nub, sortOn, (\\))
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..))
import qualified Data.Text as T
import Domain.Types.Extra.TransporterConfig (FareBreakupNode (..))
import Domain.Types.FareParameters
import Domain.Types.FarePolicy (FareChargeComponent (..))
import Kernel.Prelude
import Kernel.Types.Common (Currency, HighPrecMoney)
import Kernel.Types.Price (getAccuracy, roundAmountByCurrency')
import qualified Lib.Types.SpecialLocation as SL
import SharedLogic.FareCalculator (buildComponentMap)

data FareBreakupLine = FareBreakupLine
  { key :: Text,
    amount :: HighPrecMoney,
    subItems :: [FareBreakupLine]
  }
  deriving (Generic, Show, ToJSON)

-- | Each fare component's amount as 'SharedLogic.FareCalculator.fareSum'
-- counts it. Together they add up to 'fareSum' except for the payment
-- processing fee (shown as its own earnings rows) and the business / personal
-- discounts. Parking and its tax are 0 when the booth EDC collects parking.
-- 'overlappingComponents' are left out, since a larger component holds them.
breakupComponentAmounts :: FareParameters -> Map.Map FareChargeComponent HighPrecMoney
breakupComponentAmounts fareParams =
  let amounts = foldr Map.delete (buildComponentMap fareParams) overlappingComponents
   in if SL.edcCollectsParking fareParams.fareSettlementType
        then Map.delete ParkingChargeComponent (Map.delete ParkingTaxComponent amounts)
        else amounts

-- | Components whose amount is already part of another component, so listing
-- them would count it twice: RideVatComponent is inside GovtChargesComponent.
overlappingComponents :: [FareChargeComponent]
overlappingComponents = [RideVatComponent]

maxDepth :: Int
maxDepth = 4

leafTags :: FareBreakupNode -> [FareChargeComponent]
leafTags node = fromMaybe [] node.fareTags <> concatMap leafTags (fromMaybe [] node.subItems)

-- | Every rule a config must satisfy before it is saved or used. Empty = valid.
validateFareBreakupConfig :: FareBreakupNode -> [Text]
validateFareBreakupConfig root = nodeErrors [] 1 root <> duplicateErrors
  where
    nodeErrors path depth node =
      let here = T.intercalate " > " (reverse (node.key : path))
          tags = fromMaybe [] node.fareTags
          kids = fromMaybe [] node.subItems
          own =
            [here <> ": key must not be empty" | T.null (T.strip node.key)]
              <> [here <> ": exactly one of fareTags or subItems must be non-empty" | null tags == null kids]
              <> [here <> ": " <> show t <> " is already included in GovtChargesComponent" | t <- tags, t `elem` overlappingComponents]
              <> [here <> ": nested deeper than " <> show maxDepth <> " levels" | depth > maxDepth]
       in own <> concatMap (nodeErrors (node.key : path) (depth + 1)) kids
    allTags = leafTags root
    duplicateErrors = ["fare tag " <> show t <> " is listed in more than one leaf" | t <- nub (allTags \\ nub allTags)]

-- | The breakup lines for one ride, rounded for display; Left = show no breakup.
computeFareBreakup :: Currency -> FareBreakupNode -> FareParameters -> Either Text FareBreakupLine
computeFareBreakup currency cfg fareParams = do
  case validateFareBreakupConfig cfg of
    [] -> pure ()
    errs -> Left ("invalid fare breakup config: " <> T.intercalate "; " errs)
  let amounts = breakupComponentAmounts fareParams
      claimed = leafTags cfg
      unclaimed = [tag | (tag, amt) <- Map.toList amounts, amt /= 0, tag `notElem` claimed]
  unless (null unclaimed) $
    Left ("fare tags with an amount but in no breakup leaf: " <> T.intercalate ", " (map show unclaimed))
  let exact = exactLine amounts cfg
  pruneLine <$> apportionLine unit (roundAmountByCurrency' currency exact.amount) exact
  where
    unit = 1 / 10 ^ getAccuracy currency

exactLine :: Map.Map FareChargeComponent HighPrecMoney -> FareBreakupNode -> FareBreakupLine
exactLine amounts node = case node.subItems of
  Just kids@(_ : _) ->
    let lines' = map (exactLine amounts) kids
     in FareBreakupLine node.key (sum (map (.amount) lines')) lines'
  _ -> FareBreakupLine node.key (sum [Map.findWithDefault 0 tag amounts | tag <- fromMaybe [] node.fareTags]) []

-- | Give the line its shown amount, then split that amount across its children.
apportionLine :: HighPrecMoney -> HighPrecMoney -> FareBreakupLine -> Either Text FareBreakupLine
apportionLine unit shown line = do
  shownKids <- apportion unit shown line.subItems
  kids <- mapM (\(kid, kidShown) -> apportionLine unit kidShown kid) shownKids
  pure (FareBreakupLine line.key shown kids)

-- | Largest-remainder rounding: round every child down to the unit, then add
-- one unit to the children with the largest dropped fractions until they add
-- up to the parent's shown amount. Ties keep config order ('sortOn' is stable).
apportion :: HighPrecMoney -> HighPrecMoney -> [FareBreakupLine] -> Either Text [(FareBreakupLine, HighPrecMoney)]
apportion _ _ [] = Right []
apportion unit target kids
  | shortBy < 0 || shortBy > length kids = Left ("rounding could not reconcile " <> show target <> " across " <> show (length kids) <> " lines")
  | otherwise = Right [(kid, floored + (if idx `elem` bumped then unit else 0)) | (idx, kid, floored, _) <- rows]
  where
    rows = [(idx, kid, floorToUnit kid.amount, kid.amount - floorToUnit kid.amount) | (idx, kid) <- zip [0 :: Int ..] kids]
    shortBy = round ((target - sum [floored | (_, _, floored, _) <- rows]) / unit) :: Int
    bumped = [idx | (idx, _, _, _) <- take shortBy (sortOn (\(_, _, _, fraction) -> Down fraction) rows)]
    floorToUnit amt = fromInteger (floor (amt / unit)) * unit

-- | Hide zero lines. A level whose only visible line is a leaf is not
-- expanded, since that line would just repeat its parent's amount; a lone
-- group that still has visible lines of its own is kept with its subtree.
pruneLine :: FareBreakupLine -> FareBreakupLine
pruneLine line =
  let kids = filter (\kid -> kid.amount /= 0) (map pruneLine line.subItems)
      shown = case kids of
        [onlyKid] | null onlyKid.subItems -> []
        _ -> kids
   in FareBreakupLine line.key line.amount shown
