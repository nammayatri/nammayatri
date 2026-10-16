-- | Parts of the targeting context every feedback question has, whatever else the platform adds.
module Lib.Feedback.Context
  ( AnswerCtx (..),
    mkAnswerCtx,
  )
where

import Kernel.Prelude
import Lib.Feedback.Types (FeedbackAnswer (..))

-- | The answer, as action rules see it.
data AnswerCtx = AnswerCtx
  { selectedOptionKeys :: [Text],
    primaryOptionKey :: Maybe Text,
    text :: Maybe Text,
    number :: Maybe Double,
    rating :: Maybe Int,
    hasMedia :: Bool
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

mkAnswerCtx :: FeedbackAnswer -> AnswerCtx
mkAnswerCtx ans =
  let keys = fromMaybe [] ans.selectedOptionKeys
   in AnswerCtx
        { selectedOptionKeys = keys,
          primaryOptionKey = listToMaybe keys,
          text = ans.text,
          number = ans.number,
          rating = ans.rating,
          hasMedia = maybe False (not . null) ans.mediaFileIds
        }
