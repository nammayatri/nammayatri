module Beckn.Types.Core.Taxi.Rating.FeedbackForm where

import Data.OpenApi
import EulerHS.Prelude hiding (id)
import Kernel.Utils.Schema

data FeedbackForm = FeedbackForm
  { question :: Text,
    answer :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema FeedbackForm where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
