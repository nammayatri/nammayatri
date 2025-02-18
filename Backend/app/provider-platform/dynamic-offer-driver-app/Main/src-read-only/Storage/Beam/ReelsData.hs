{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.ReelsData where

import qualified Data.Aeson
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.ReelsData
import Kernel.External.Encryption
import qualified Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data ReelsDataT f = ReelsDataT
  { bottomButtonConfig :: B.C f Data.Aeson.Value,
    carouselBigImageUrl :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    carouselSmallImageUrl :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    carouselTextColor :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    carouselTextString :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    description :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    id :: B.C f Kernel.Prelude.Text,
    language :: B.C f Kernel.External.Types.Language,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    rank :: B.C f Kernel.Prelude.Int,
    reelKey :: B.C f Kernel.Prelude.Text,
    shareLink :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    sideButtonConfig :: B.C f Data.Aeson.Value,
    thresholdConfig :: B.C f (Kernel.Prelude.Maybe Domain.Types.ReelsData.ReelVideoThresholdConfig),
    thumbnailImageUrl :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    title :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    videoUrl :: B.C f Kernel.Prelude.Text,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table ReelsDataT where
  data PrimaryKey ReelsDataT f = ReelsDataId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = ReelsDataId . id

type ReelsData = ReelsDataT Identity

$(enableKVPG ''ReelsDataT ['id] [])

$(mkTableInstances ''ReelsDataT "reels_data")
