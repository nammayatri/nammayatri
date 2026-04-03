{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.Exophone where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Data.Text
import qualified Kernel.External.Call.Types
import qualified Domain.Types.Exophone
import qualified Kernel.Prelude
import qualified Database.Beam as B



data ExophoneT f
    = ExophoneT {backupPhone :: (B.C f Data.Text.Text),
                 callService :: (B.C f Kernel.External.Call.Types.CallService),
                 exophoneType :: (B.C f Domain.Types.Exophone.ExophoneType),
                 id :: (B.C f Data.Text.Text),
                 isPrimaryDown :: (B.C f Kernel.Prelude.Bool),
                 merchantId :: (B.C f Data.Text.Text),
                 merchantOperatingCityId :: (B.C f Data.Text.Text),
                 primaryPhone :: (B.C f Data.Text.Text),
                 createdAt :: (B.C f Kernel.Prelude.UTCTime),
                 updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table ExophoneT
    where data PrimaryKey ExophoneT f = ExophoneId (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
          primaryKey = ExophoneId . id
type Exophone = ExophoneT Identity

$(enableKVPG (''ExophoneT) [('id)] [[('backupPhone)], [('primaryPhone)]])

$(mkTableInstances (''ExophoneT) "exophone")

