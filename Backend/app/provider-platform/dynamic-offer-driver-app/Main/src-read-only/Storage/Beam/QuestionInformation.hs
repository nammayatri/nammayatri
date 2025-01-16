{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.QuestionInformation where

import qualified Data.Aeson
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.LmsEnumTypes
import qualified Domain.Types.QuestionInformation
import Kernel.External.Encryption
import qualified Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data QuestionInformationT f = QuestionInformationT
  { language :: B.C f Kernel.External.Types.Language,
    options :: B.C f Data.Aeson.Value,
    question :: B.C f Domain.Types.LmsEnumTypes.QuizQuestion,
    questionId :: B.C f Kernel.Prelude.Text,
    questionType :: B.C f Domain.Types.QuestionInformation.QuizQuestionType,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table QuestionInformationT where
  data PrimaryKey QuestionInformationT f = QuestionInformationId (B.C f Kernel.External.Types.Language) (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = QuestionInformationId <$> language <*> questionId

type QuestionInformation = QuestionInformationT Identity

$(enableKVPG ''QuestionInformationT ['language, 'questionId] [])

$(mkTableInstances ''QuestionInformationT "question_information")
