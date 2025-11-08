{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Translations where

import qualified Data.Text
import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import qualified Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data TranslationsT f = TranslationsT
  { createdAt :: B.C f Kernel.Prelude.UTCTime,
    id :: B.C f Data.Text.Text,
    language :: B.C f Kernel.External.Types.Language,
    merchantOperatingCityId :: B.C f Data.Text.Text,
    message :: B.C f Data.Text.Text,
    messageKey :: B.C f Data.Text.Text,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table TranslationsT where
  data PrimaryKey TranslationsT f = TranslationsId (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
  primaryKey = TranslationsId . id

type Translations = TranslationsT Identity

$(enableKVPG ''TranslationsT ['id] [['language], ['messageKey]])

$(mkTableInstances ''TranslationsT "translations")
