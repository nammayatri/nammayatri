module SharedLogic.AppFeedback.Placement
  ( placementSubject,
    placementInfo,
  )
where

import Domain.Types.AppFeedbackConfig (AppFeedbackPlacement (..))
import Domain.Types.AppFeedbackResponse (AppFeedbackSubjectType (..))
import Kernel.Prelude

-- | What answers at a placement are about. Exhaustive, so a new placement does not compile until
-- its subject is decided here (and, for a new subject type, loaded in SharedLogic.AppFeedback.Context).
placementSubject :: AppFeedbackPlacement -> AppFeedbackSubjectType
placementSubject = \case
  HOME -> RIDER
  SEARCH_RESULTS -> RIDER
  TICKET_BOOKED -> FRFS_BOOKING
  JOURNEY_COMPLETED -> JOURNEY

-- | Every placement with its subject type, in declaration order (for the dashboard).
placementInfo :: [(AppFeedbackPlacement, AppFeedbackSubjectType)]
placementInfo = [(p, placementSubject p) | p <- [minBound .. maxBound]]
