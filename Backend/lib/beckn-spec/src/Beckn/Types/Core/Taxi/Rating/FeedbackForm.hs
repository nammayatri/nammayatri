{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE InstanceSigs #-}

module Beckn.Types.Core.Taxi.Rating.FeedbackForm where

import Beckn.Types.Core.Taxi.Rating.Category (CategoryName)
import Data.Aeson as A
import Data.Aeson.Types
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

data AnswerType = TEXT deriving (Generic, Show, Read, ToSchema)

instance ToJSON AnswerType where
  toJSON TEXT = A.String "text"

instance FromJSON AnswerType where
  parseJSON :: Value -> Parser AnswerType
  parseJSON (A.String "text") = pure TEXT
  parseJSON invalid =
    prependFailure
      "parsing AnswerType failed, "
      (typeMismatch "Object" invalid)

data FeedbackFormAPIEntity = FeedbackFormAPIEntity
  { id :: Int,
    question :: Text,
    answer_type :: AnswerType
  }
  deriving (Generic, Show, Read, ToSchema, ToJSON, FromJSON)

data GetFeedbackFormMes = GetFeedbackFormMes
  { rating_value :: Int,
    rating_category :: CategoryName
  }
  deriving (Generic, Show, Read, ToSchema, ToJSON, FromJSON)
