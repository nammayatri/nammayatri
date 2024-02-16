{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.LmsVideoTranslation where

import qualified Database.Beam as B
import qualified Domain.Types.LmsModuleVideoInformation
import qualified Domain.Types.LmsVideoTranslation
import Kernel.External.Encryption
import qualified Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Tools.Beam.UtilsTH

data LmsVideoTranslationT f = LmsVideoTranslationT
  { bottomButtonConfig :: B.C f [Domain.Types.LmsVideoTranslation.ReelRowButtonConfig],
    completedThresholdInPercentage :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    completedWatchCount :: B.C f Kernel.Prelude.Int,
    description :: B.C f Kernel.Prelude.Text,
    duration :: B.C f Kernel.Prelude.Int,
    language :: B.C f Kernel.External.Types.Language,
    sideButtonConfig :: B.C f [Domain.Types.LmsVideoTranslation.ReelRowButtonConfig],
    startThresholdInPercentage :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    thresholdEnabled :: B.C f Kernel.Prelude.Bool,
    thumbnailImage :: B.C f Kernel.Prelude.Text,
    title :: B.C f Kernel.Prelude.Text,
    url :: B.C f Kernel.Prelude.Text,
    videoId :: B.C f Kernel.Prelude.Text,
    viewCount :: B.C f Kernel.Prelude.Int,
    ytVideoId :: B.C f Kernel.Prelude.Text,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table LmsVideoTranslationT where
  data PrimaryKey LmsVideoTranslationT f = LmsVideoTranslationId (B.C f Kernel.External.Types.Language) (B.C f Kernel.Prelude.Text)
    deriving (Generic, B.Beamable)
  primaryKey = LmsVideoTranslationId <$> language <*> videoId

type LmsVideoTranslation = LmsVideoTranslationT Identity

$(enableKVPG ''LmsVideoTranslationT ['language, 'videoId] [])

$(mkTableInstances ''LmsVideoTranslationT "lms_video_translation")
