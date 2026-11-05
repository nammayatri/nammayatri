-- | The rider no-show count on a booking and what a close does at the cap.
module SharedLogic.SharedCab.NoShow
  ( noShowsAfter,
    NoShowAction (..),
    afterNoShow,
    actionAfterClose,
  )
where

import Kernel.Prelude
import SharedLogic.SharedCab.Allocation.Types (Blame (..))

noShowsAfter :: Blame -> Int -> Int
noShowsAfter BlameRider n = n + 1
noShowsAfter _ n = n

data NoShowAction = Reallocate | AutoCancel
  deriving (Show, Eq)

afterNoShow :: Int -> Int -> NoShowAction
afterNoShow maxNoShows noShows
  | noShows >= maxNoShows = AutoCancel
  | otherwise = Reallocate

-- | What a close that blamed `blame` does to a booking with `noShows` already booked: only the rider's own no-show can
-- cancel it, and only if it takes the count to the cap.
actionAfterClose :: Blame -> Int -> Int -> NoShowAction
actionAfterClose blame maxNoShows noShows = case blame of
  BlameRider -> afterNoShow maxNoShows (noShowsAfter BlameRider noShows)
  BlameDriver -> Reallocate
  BlameNone -> Reallocate
