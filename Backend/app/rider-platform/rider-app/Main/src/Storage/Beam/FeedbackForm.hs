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
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.FeedbackForm where

import Data.Serialize
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres
  ( Postgres,
  )
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Domain.Types.FeedbackForm as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import EulerHS.PIIEncryption
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
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

type FeedbackForm = FeedbackFormT Identity

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

$(enableKVPG ''FeedbackFormT ['id] [])

$(mkTableInstances ''FeedbackFormT "feedback_form" "atlas_app")
