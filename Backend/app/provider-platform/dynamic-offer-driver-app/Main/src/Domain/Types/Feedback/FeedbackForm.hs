{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}

module Domain.Types.Feedback.FeedbackForm where

import qualified Database.Beam as B
import Database.Beam.Backend (BeamSqlBackend, FromBackendRow, HasSqlValueSyntax, autoSqlValueSyntax)
import Database.Beam.Backend.SQL.SQL2003 (HasSqlValueSyntax (sqlValueSyntax))
import Database.Beam.Postgres (Postgres)
import Database.PostgreSQL.Simple.FromField (FromField (fromField))
import Domain.Types.Ride (Ride)
import Kernel.Prelude
import Kernel.Types.Common (fromFieldEnum)
import Kernel.Types.Id

data FeedbackFormReq = FeedbackFormReq
  { rideId :: Id Ride,
    feedback :: [FeedbackAnswer]
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data FeedbackAnswer = FeedbackAnswer
  { questionId :: Text,
    answer :: [Text]
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

newtype FeedbackFormList = FeedbackFormList {_data :: [FeedbackFormAPIEntity]}
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema, Eq)

data FeedbackFormAPIEntity = FeedbackFormAPIEntity
  { categoryName :: Category,
    feedbackForm :: [FeedbackFormItem]
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema, Eq)

data Category = RIDE | DRIVER | VEHICLE
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema, Eq, Read)

instance FromField Category where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Category where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Category

instance FromBackendRow Postgres Category

instance IsString Category where
  fromString = show

deriving stock instance Ord Category

data FeedbackFormItem = FeedbackFormItem
  { id :: Id FeedbackFormItem,
    rating :: Maybe Int,
    question :: Text,
    answer :: [Text],
    answerType :: AnswerType
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema, Eq)

data AnswerType = Text | Checkbox | Radio
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema, Eq, Read)

instance FromField AnswerType where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be AnswerType where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be AnswerType

instance FromBackendRow Postgres AnswerType

instance IsString AnswerType where
  fromString = show

deriving stock instance Ord AnswerType

data FeedbackFormRes = FeedbackFormRes
  { categoryName :: Category,
    id :: Id FeedbackFormItem,
    rating :: Maybe Int,
    question :: Text,
    answer :: [Text],
    answerType :: AnswerType
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema, Eq)
