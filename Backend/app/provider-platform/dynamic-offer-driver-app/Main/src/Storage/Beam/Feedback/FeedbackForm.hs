{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.Feedback.FeedbackForm where

import qualified Data.Aeson as A
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres
  ( Postgres,
  )
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Domain.Types.Feedback.FeedbackForm as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.UtilsTH
import Sequelize

instance FromField Domain.Category where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.Category where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.Category

instance FromBackendRow Postgres Domain.Category

instance IsString Domain.Category where
  fromString = show

instance FromField Domain.AnswerType where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.AnswerType where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.AnswerType

instance FromBackendRow Postgres Domain.AnswerType

instance IsString Domain.AnswerType where
  fromString = show

data FeedbackFormT f = FeedbackFormT
  { id :: B.C f Text,
    categoryName :: B.C f Domain.Category,
    rating :: B.C f (Maybe Int),
    question :: B.C f Text,
    answer :: B.C f [Text],
    answerType :: B.C f Domain.AnswerType
  }
  deriving (Generic, B.Beamable)

instance B.Table FeedbackFormT where
  data PrimaryKey FeedbackFormT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta FeedbackFormT where
  modelFieldModification = feedbackFormTMod
  modelTableName = "feedback_form"
  modelSchemaName = Just "atlas_driver_offer_bpp"

type FeedbackForm = FeedbackFormT Identity

instance FromJSON FeedbackForm where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON FeedbackForm where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show FeedbackForm

feedbackFormTMod :: FeedbackFormT (B.FieldModification (B.TableField FeedbackFormT))
feedbackFormTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      categoryName = B.fieldNamed "category_name",
      rating = B.fieldNamed "rating",
      question = B.fieldNamed "question",
      answer = B.fieldNamed "answer",
      answerType = B.fieldNamed "answer_type"
    }

instance Serialize FeedbackForm where
  put = error "undefined"
  get = error "undefined"

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

feedbackFormToHSModifiers :: M.Map Text (A.Value -> A.Value)
feedbackFormToHSModifiers =
  M.empty

feedbackFormToPSModifiers :: M.Map Text (A.Value -> A.Value)
feedbackFormToPSModifiers =
  M.empty

$(enableKVPG ''FeedbackFormT ['id] [['rating]]) -- DON'T Enable for KV
