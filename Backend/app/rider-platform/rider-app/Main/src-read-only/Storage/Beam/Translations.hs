{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.Translations where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Kernel.External.Types
import qualified Data.Text
import qualified Database.Beam as B



data TranslationsT f
    = TranslationsT {createdAt :: (B.C f Kernel.Prelude.UTCTime),
                     id :: (B.C f Data.Text.Text),
                     language :: (B.C f Kernel.External.Types.Language),
                     merchantOperatingCityId :: (B.C f Data.Text.Text),
                     message :: (B.C f Data.Text.Text),
                     messageKey :: (B.C f Data.Text.Text),
                     updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table TranslationsT
    where data PrimaryKey TranslationsT f = TranslationsId (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
          primaryKey = TranslationsId . id
type Translations = TranslationsT Identity

$(enableKVPG (''TranslationsT) [('id)] [[('language)], [('messageKey)]])

$(mkTableInstances (''TranslationsT) "translations")

