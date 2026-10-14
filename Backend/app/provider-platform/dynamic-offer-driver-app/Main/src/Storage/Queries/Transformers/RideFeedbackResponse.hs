module Storage.Queries.Transformers.RideFeedbackResponse where

import Domain.Types.RideFeedbackResponse (RideFeedbackAnswer (..))
import Kernel.Prelude

-- | The answer from its typed columns: Nothing when none is set (a response that was only shown,
-- skipped or expired).
mkAnswer :: Maybe [Text] -> Maybe Text -> Maybe Double -> Maybe Int -> Maybe [Text] -> Maybe RideFeedbackAnswer
mkAnswer selectedOptionKeys text number rating mediaFileIds
  | isNothing selectedOptionKeys && isNothing text && isNothing number && isNothing rating && isNothing mediaFileIds = Nothing
  | otherwise = Just RideFeedbackAnswer {..}
