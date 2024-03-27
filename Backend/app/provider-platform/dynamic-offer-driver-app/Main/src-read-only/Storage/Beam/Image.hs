{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Image where

import qualified Database.Beam as B
import qualified Domain.Types.Image
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH
import qualified Tools.Error

data ImageT f = ImageT
  { failureReason :: (B.C f (Kernel.Prelude.Maybe Tools.Error.DriverOnboardingError)),
    id :: (B.C f Kernel.Prelude.Text),
    imageType :: (B.C f Domain.Types.Image.ImageType),
    isValid :: (B.C f Kernel.Prelude.Bool),
    merchantId :: (B.C f Kernel.Prelude.Text),
    personId :: (B.C f Kernel.Prelude.Text),
    s3Path :: (B.C f Kernel.Prelude.Text),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table ImageT where
  data PrimaryKey ImageT f = ImageId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = ImageId . id

type Image = ImageT Identity

$(enableKVPG (''ImageT) [('id)] [[('personId)]])

$(mkTableInstances (''ImageT) "image")
