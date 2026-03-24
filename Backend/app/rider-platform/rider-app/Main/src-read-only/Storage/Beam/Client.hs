{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.Client where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Database.Beam as B



data ClientT f
    = ClientT {id :: (B.C f Kernel.Prelude.Text),
               shortId :: (B.C f Kernel.Prelude.Text),
               merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
               createdAt :: (B.C f Kernel.Prelude.UTCTime),
               updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table ClientT
    where data PrimaryKey ClientT f = ClientId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = ClientId . id
type Client = ClientT Identity

$(enableKVPG (''ClientT) [('id)] [])

$(mkTableInstances (''ClientT) "client")

