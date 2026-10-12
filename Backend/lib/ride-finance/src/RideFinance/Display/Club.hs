module RideFinance.Display.Club
  ( Keys (..),
    noKeys,
    classify,
    RowOps (..),
    clubRows,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Kernel.Prelude
import RideFinance.Core.Components (AmountPart (..), FareComponent, allComponents)
import RideFinance.Core.Types (ClubPolicy)

data Keys k = Keys
  { taxExclusiveKeys :: [k],
    taxKeys :: [k],
    clubbedKey :: Maybe k
  }
  deriving (Show, Eq, Generic)

noKeys :: Keys k
noKeys = Keys {taxExclusiveKeys = [], taxKeys = [], clubbedKey = Nothing}

classify :: Eq k => (FareComponent -> Keys k) -> k -> Maybe (FareComponent, AmountPart)
classify keysOf key =
  listToMaybe
    [ (c, amountPart)
      | c <- allComponents,
        let keys = keysOf c,
        (amountPart, candidates) <- [(TaxExclusivePart, keys.taxExclusiveKeys), (TaxPart, keys.taxKeys), (TaxInclusivePart, maybeToList keys.clubbedKey)],
        key `elem` candidates
    ]

data RowOps k row = RowOps
  { rowKey :: row -> Maybe k,
    rowGroup :: row -> Maybe Text,
    addAmounts :: row -> row -> row,
    withKey :: k -> row -> row
  }

clubRows :: Eq k => (FareComponent -> Keys k) -> RowOps k row -> ClubPolicy -> [row] -> [row]
clubRows keysOf ops policy rows
  | Set.null policy = rows
  | otherwise = catMaybes (zipWith emit [0 :: Int ..] tagged)
  where
    tagged = [(row, clubbedBucket row) | row <- rows]
    clubbedBucket row = do
      (c, amountPart) <- classify keysOf =<< ops.rowKey row
      guard (Set.member c policy)
      pure ((c, ops.rowGroup row), amountPart)
    buckets =
      Map.fromListWith
        (flip (<>))
        [(bucket, [(index, row, amountPart)]) | (index, (row, Just (bucket, amountPart))) <- zip [0 :: Int ..] tagged]
    emit index (row, mbBucket) = case mbBucket of
      Nothing -> Just row
      Just (bucket, _) -> case Map.findWithDefault [] bucket buckets of
        members@((firstIndex, firstRow, _) : _)
          | index == firstIndex -> Just (mergeBucket (fst bucket) firstIndex firstRow members)
        _ -> Nothing
    mergeBucket c firstIndex firstRow members =
      let (anchorIndex, anchor) = fromMaybe (firstIndex, firstRow) (listToMaybe [(memberIndex, member) | (memberIndex, member, TaxExclusivePart) <- members])
          merged = foldl' ops.addAmounts anchor [member | (memberIndex, member, _) <- members, memberIndex /= anchorIndex]
       in maybe merged (\renamedKey -> ops.withKey renamedKey merged) (keysOf c).clubbedKey
