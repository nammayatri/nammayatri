module SharedLogic.FareBreakup
  ( FareBreakupLine (..),
    computeFareBreakup,
  )
where

import Data.List (nub, nubBy, sortOn, (\\))
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..))
import qualified Data.Text as T
import Domain.Types.Extra.TransporterConfig (FareBreakupNode (..), FareBreakupNodeValue (..))
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

maxDepth :: Int
maxDepth = 4

computeFareBreakup :: Currency -> [FareBreakupNode] -> FareParameters -> [(FareChargeComponent, HighPrecMoney)] -> [FareBreakupLine]
computeFareBreakup currency configuredSubItems fareParams extraTagAmounts =
  pruneLines (apportionLines unit (roundAmountByCurrency' currency exactTotal) exactLines)
  where
    unit = 1 / 10 ^ getAccuracy currency
    amounts = Map.union (Map.fromList extraTagAmounts) (breakupComponentAmounts fareParams)
    exactLines = map (exactLine amounts) (snd (resolveFirstOccurrences 1 [] configuredSubItems))
    exactTotal = sum (map (.amount) exactLines)

breakupComponentAmounts :: FareParameters -> Map.Map FareChargeComponent HighPrecMoney
breakupComponentAmounts fareParams =
  let amounts = buildComponentMap fareParams
   in if SL.edcCollectsParking fareParams.fareSettlementType
        then foldr Map.delete amounts [ParkingChargeComponent, ParkingTaxComponent]
        else amounts

resolveFirstOccurrences :: Int -> [FareChargeComponent] -> [FareBreakupNode] -> ([FareChargeComponent], [FareBreakupNode])
resolveFirstOccurrences depth seenTags nodes
  | depth > maxDepth = (seenTags, [])
  | otherwise = mapAccumL (resolveNode depth) seenTags (nubBy ((==) `on` (.key)) (filter (not . T.null . T.strip . (.key)) nodes))

resolveNode :: Int -> [FareChargeComponent] -> FareBreakupNode -> ([FareChargeComponent], FareBreakupNode)
resolveNode depth seenTags entry = case entry.node of
  FareTags configuredTags ->
    let tags = nub configuredTags \\ seenTags
     in (seenTags <> tags, FareBreakupNode {key = entry.key, node = FareTags tags})
  SubItems kids ->
    let (seenAfterKids, resolvedKids) = resolveFirstOccurrences (depth + 1) seenTags kids
     in (seenAfterKids, FareBreakupNode {key = entry.key, node = SubItems resolvedKids})

exactLine :: Map.Map FareChargeComponent HighPrecMoney -> FareBreakupNode -> FareBreakupLine
exactLine amounts entry = case entry.node of
  SubItems kids ->
    let lines' = map (exactLine amounts) kids
     in FareBreakupLine entry.key (sum (map (.amount) lines')) lines'
  FareTags tags -> FareBreakupLine entry.key (sum [Map.findWithDefault 0 tag amounts | tag <- tags]) []

apportionLines :: HighPrecMoney -> HighPrecMoney -> [FareBreakupLine] -> [FareBreakupLine]
apportionLines unit target exactLines =
  [FareBreakupLine line.key (floored + (if idx `elem` bumped then unit else 0)) (apportionLines unit (floored + (if idx `elem` bumped then unit else 0)) line.subItems) | (idx, line, floored, _) <- rows]
  where
    floorToUnit amt = fromInteger (floor (amt / unit)) * unit
    rows = [(idx, line, floorToUnit line.amount, line.amount - floorToUnit line.amount) | (idx, line) <- zip [0 :: Int ..] exactLines]
    shortBy = round ((target - sum [floored | (_, _, floored, _) <- rows]) / unit) :: Int
    bumped = [idx | (idx, _, _, _) <- take shortBy (sortOn (\(_, _, _, fraction) -> Down fraction) rows)]

pruneLines :: [FareBreakupLine] -> [FareBreakupLine]
pruneLines lines' = [FareBreakupLine line.key line.amount (pruneLines line.subItems) | line <- lines', line.amount /= 0]
