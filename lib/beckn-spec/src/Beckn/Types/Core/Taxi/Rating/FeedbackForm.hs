module Beckn.Types.Core.Taxi.Rating.FeedbackForm where

import Beckn.Utils.Schema
import Data.OpenApi
import EulerHS.Prelude hiding (id)

data FeedbackForm = FeedbackForm
  { question :: Text,
    answer :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema FeedbackForm where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
