module Storage.Queries.Transformers.JourneyLegsFeedback where

import qualified Data.Aeson as A
import qualified Domain.Types.JourneyLegsFeedbacks as Domain
import Kernel.Prelude
import Kernel.Utils.JSON (valueToMaybe)

getFeedbackData :: A.Value -> Maybe Domain.JourneyLegFeedbackData
getFeedbackData feedbackData = Kernel.Utils.JSON.valueToMaybe feedbackData

getFeedbackDataJson :: Domain.JourneyLegFeedbackData -> A.Value
getFeedbackDataJson = \case
  Domain.Taxi feedbackData -> toJSON feedbackData
  Domain.Bus feedbackData -> toJSON feedbackData
  Domain.Walk feedbackData -> toJSON feedbackData
  Domain.Subway feedbackData -> toJSON feedbackData
  Domain.Metro feedbackData -> toJSON feedbackData
