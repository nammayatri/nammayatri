{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.LmsVideoTranslation where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Kernel.External.Types
import qualified Data.Aeson
import qualified Database.Beam as B



data LmsVideoTranslationT f
    = LmsVideoTranslationT {bottomButtonConfig :: (B.C f Data.Aeson.Value),
                            completedThresholdInPercentage :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                            completedWatchCount :: (B.C f Kernel.Prelude.Int),
                            description :: (B.C f Kernel.Prelude.Text),
                            duration :: (B.C f Kernel.Prelude.Int),
                            language :: (B.C f Kernel.External.Types.Language),
                            sideButtonConfig :: (B.C f Data.Aeson.Value),
                            startThresholdInPercentage :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                            thresholdEnabled :: (B.C f Kernel.Prelude.Bool),
                            thumbnailImage :: (B.C f Kernel.Prelude.Text),
                            title :: (B.C f Kernel.Prelude.Text),
                            url :: (B.C f Kernel.Prelude.Text),
                            useMerchantOperatingCityDefaultLanguageVideoUrl :: (B.C f Kernel.Prelude.Bool),
                            videoId :: (B.C f Kernel.Prelude.Text),
                            viewCount :: (B.C f Kernel.Prelude.Int),
                            ytVideoId :: (B.C f Kernel.Prelude.Text),
                            createdAt :: (B.C f Kernel.Prelude.UTCTime),
                            updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table LmsVideoTranslationT
    where data PrimaryKey LmsVideoTranslationT f = LmsVideoTranslationId (B.C f Kernel.External.Types.Language) (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = LmsVideoTranslationId <$> language <*> videoId
type LmsVideoTranslation = LmsVideoTranslationT Identity

$(enableKVPG (''LmsVideoTranslationT) [('language), ('videoId)] [])

$(mkTableInstances (''LmsVideoTranslationT) "lms_video_translation")

