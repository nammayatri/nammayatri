{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.AiVerificationQuestion where

import qualified Data.Aeson
import qualified Database.Beam as B
import qualified Domain.Types.AiVerificationQuestion
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data AiVerificationQuestionT f = AiVerificationQuestionT
  { enabled :: (B.C f Kernel.Prelude.Bool),
    entityType :: (B.C f Kernel.Prelude.Text),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    options :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
    question :: (B.C f Kernel.Prelude.Text),
    questionLabel :: (B.C f Kernel.Prelude.Text),
    questionType :: (B.C f Domain.Types.AiVerificationQuestion.AiQuestionType),
    threshold :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table AiVerificationQuestionT where
  data PrimaryKey AiVerificationQuestionT f = AiVerificationQuestionId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = AiVerificationQuestionId . id

type AiVerificationQuestion = AiVerificationQuestionT Identity

$(enableKVPG (''AiVerificationQuestionT) [('id)] [])

$(mkTableInstances (''AiVerificationQuestionT) "ai_verification_question")
