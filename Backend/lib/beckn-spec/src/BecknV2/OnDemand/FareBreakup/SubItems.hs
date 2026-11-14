module BecknV2.OnDemand.FareBreakup.SubItems
  ( FareBreakupNode (..),
    FareBreakupNodeValue (..),
    FareBreakupLine (..),
    computeFareBreakupLines,
    FareUISection (..),
    FareUIComponentType (..),
    FareUISubItem (..),
    FareUIComponent (..),
  )
where

import Data.Aeson
import Data.List (nub, nubBy, sortOn, (\\))
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..))
import qualified Data.Text as T
import Kernel.Prelude
import Kernel.Types.Common (Currency, HighPrecMoney, PriceAPIEntity)
import Kernel.Types.Price (getAccuracy, roundAmountByCurrency')

data FareBreakupNode tag = FareBreakupNode
  { key :: Text,
    node :: FareBreakupNodeValue tag
  }
  deriving (Generic, Show, Eq)

instance ToJSON tag => ToJSON (FareBreakupNode tag)

instance FromJSON tag => FromJSON (FareBreakupNode tag)

instance ToSchema tag => ToSchema (FareBreakupNode tag)

data FareBreakupNodeValue tag
  = FareTags [tag]
  | SubItems [FareBreakupNode tag]
  deriving (Generic, Show, Eq)

instance ToJSON tag => ToJSON (FareBreakupNodeValue tag) where
  toJSON = genericToJSON defaultOptions {sumEncoding = UntaggedValue}

instance FromJSON tag => FromJSON (FareBreakupNodeValue tag) where
  parseJSON = genericParseJSON defaultOptions {sumEncoding = UntaggedValue}

instance ToSchema tag => ToSchema (FareBreakupNodeValue tag)

data FareBreakupLine = FareBreakupLine
  { key :: Text,
    amount :: HighPrecMoney,
    subItems :: [FareBreakupLine]
  }
  deriving (Generic, Show, ToJSON)

data FareUISection
  = FareBreakup
  | Footnote
  deriving stock (Show, Eq, Ord, Generic, Enum, Bounded)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data FareUIComponentType = Earning | Deduction
  deriving stock (Show, Eq, Ord, Generic, Enum, Bounded)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data FareUISubItem = FareUISubItem
  { title :: Text,
    price :: PriceAPIEntity,
    uiTranslation :: Maybe Text,
    _type :: FareUIComponentType,
    subItems :: [FareUISubItem]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data FareUIComponent = FareUIComponent
  { title :: Text,
    price :: PriceAPIEntity,
    uiTranslation :: Maybe Text,
    section :: FareUISection,
    _type :: FareUIComponentType,
    subItems :: [FareUISubItem]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

maxDepth :: Int
maxDepth = 4

computeFareBreakupLines :: Ord tag => Currency -> [FareBreakupNode tag] -> Map.Map tag HighPrecMoney -> [FareBreakupLine]
computeFareBreakupLines currency configuredSubItems amounts =
  pruneLines (apportionLines unit (roundAmountByCurrency' currency exactTotal) exactLines)
  where
    unit = 1 / 10 ^ getAccuracy currency
    exactLines = map (exactLine amounts) (snd (resolveFirstOccurrences 1 [] configuredSubItems))
    exactTotal = sum (map (.amount) exactLines)

resolveFirstOccurrences :: Eq tag => Int -> [tag] -> [FareBreakupNode tag] -> ([tag], [FareBreakupNode tag])
resolveFirstOccurrences depth seenTags nodes
  | depth > maxDepth = (seenTags, [])
  | otherwise = mapAccumL (resolveNode depth) seenTags (nubBy ((==) `on` (.key)) (filter (not . T.null . T.strip . (.key)) nodes))

resolveNode :: Eq tag => Int -> [tag] -> FareBreakupNode tag -> ([tag], FareBreakupNode tag)
resolveNode depth seenTags entry = case entry.node of
  FareTags configuredTags ->
    let tags = nub configuredTags \\ seenTags
     in (seenTags <> tags, FareBreakupNode {key = entry.key, node = FareTags tags})
  SubItems kids ->
    let (seenAfterKids, resolvedKids) = resolveFirstOccurrences (depth + 1) seenTags kids
     in (seenAfterKids, FareBreakupNode {key = entry.key, node = SubItems resolvedKids})

exactLine :: Ord tag => Map.Map tag HighPrecMoney -> FareBreakupNode tag -> FareBreakupLine
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
