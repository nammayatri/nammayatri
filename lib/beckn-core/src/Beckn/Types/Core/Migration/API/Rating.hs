module Beckn.Types.Core.Migration.API.Rating where

import Beckn.Types.Core.Ack
import Beckn.Types.Core.Migration.API.Types (BecknCallbackReq, BecknReq)
import Beckn.Types.Core.Migration.Feedback (Feedback)
import Data.Aeson (withObject, (.:))
import Data.Aeson.Types (parseFail)
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))

type RatingAPI =
  "rating"
    :> ReqBody '[JSON] (BecknReq RatingInfo)
    :> Post '[JSON] AckResponse

ratingAPI :: Proxy RatingAPI
ratingAPI = Proxy

data RatingInfo = RatingInfo
  { id :: Text,
    value :: Int
  }
  deriving (Generic, Show, ToJSON)

instance FromJSON RatingInfo where
  parseJSON = withObject "RatingInfo" $ \obj -> do
    val <- obj .: "value"
    unless (val >= 1 && val <= 5) $ parseFail "Expected value to be from 1 to 5."
    ratedId <- obj .: "id"
    pure $ RatingInfo ratedId val

type OnRatingAPI =
  "on_rating"
    :> ReqBody '[JSON] (BecknCallbackReq FeedbackObject)
    :> Post '[JSON] AckResponse

onRatingAPI :: Proxy OnRatingAPI
onRatingAPI = Proxy

newtype FeedbackObject = FeedbackObject
  { feedback :: Feedback
  }
  deriving (Generic, Show, FromJSON, ToJSON)
