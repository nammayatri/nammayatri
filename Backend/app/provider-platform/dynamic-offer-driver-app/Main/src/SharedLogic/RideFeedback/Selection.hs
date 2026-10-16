module SharedLogic.RideFeedback.Selection
  ( RideFeedbackSelection,
    QuestionSelection (..),
    selectQuestions,
  )
where

import qualified Domain.Types.MerchantOperatingCity as DMOC
import Environment
import Kernel.Prelude
import Kernel.Types.Id
import Lib.Feedback.Selection (FeedbackSelection, QuestionSelection (..))
import qualified Lib.Feedback.Selection as LFS
import qualified Lib.Yudhishthira.Tools.DebugLog as LYDL
import qualified Lib.Yudhishthira.Types as LYT
import SharedLogic.RideFeedback.Context (RideFeedbackContext)
import Storage.Beam.Yudhishthira ()

-- | Output of the RIDE-FEEDBACK logic (app_dynamic_logic_element): the keys of the questions this
-- ride should get. Content, timing and limits of each key come from ride_feedback_config.
type RideFeedbackSelection = FeedbackSelection

-- | Runs the city's RIDE-FEEDBACK logic on the ride context (see 'LFS.selectQuestions').
selectQuestions ::
  Id DMOC.MerchantOperatingCity ->
  -- | Stable A/B key: the rider's BPP id, or the booking id when the rider is unknown.
  Text ->
  UTCTime ->
  RideFeedbackContext ->
  Flow QuestionSelection
selectQuestions merchantOpCityId = LFS.selectQuestions LYDL.Driver LYT.RIDE_FEEDBACK (cast merchantOpCityId)
