{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Faq where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import qualified Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data FaqT f = FaqT
  { answer :: (B.C f Kernel.Prelude.Text),
    category :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    faqGroupId :: (B.C f Kernel.Prelude.Text),
    id :: (B.C f Kernel.Prelude.Text),
    language :: (B.C f Kernel.External.Types.Language),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    question :: (B.C f Kernel.Prelude.Text),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table FaqT where
  data PrimaryKey FaqT f = FaqId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FaqId . id

type Faq = FaqT Identity

$(enableKVPG (''FaqT) [('id)] [[('faqGroupId)], [('language)]])

$(mkTableInstances (''FaqT) "faq")
