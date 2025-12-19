{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FeedbackForm where

import qualified Data.Aeson
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.FeedbackForm
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data FeedbackFormT f = FeedbackFormT
  { answer :: B.C f [Kernel.Prelude.Text],
    answerType :: B.C f Domain.Types.FeedbackForm.AnswerType,
    badges :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    categoryName :: B.C f Domain.Types.FeedbackForm.Category,
    id :: B.C f Kernel.Prelude.Text,
    question :: B.C f Kernel.Prelude.Text,
    questionTranslations :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    rating :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table FeedbackFormT where
  data PrimaryKey FeedbackFormT f = FeedbackFormId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FeedbackFormId . id

type FeedbackForm = FeedbackFormT Identity

$(enableKVPG ''FeedbackFormT ['id] [])

$(mkTableInstances ''FeedbackFormT "feedback_form")
