{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.WhiteListOrg where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Domain
import qualified Database.Beam as B



data WhiteListOrgT f
    = WhiteListOrgT {createdAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
                     domain :: (B.C f Kernel.Types.Beckn.Domain.Domain),
                     id :: (B.C f Kernel.Prelude.Text),
                     merchantId :: (B.C f Kernel.Prelude.Text),
                     merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
                     subscriberId :: (B.C f Kernel.Prelude.Text),
                     updatedAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime))}
    deriving (Generic, B.Beamable)
instance B.Table WhiteListOrgT
    where data PrimaryKey WhiteListOrgT f = WhiteListOrgId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = WhiteListOrgId . id
type WhiteListOrg = WhiteListOrgT Identity

$(enableKVPG (''WhiteListOrgT) [('id)] [])

$(mkTableInstancesWithTModifier (''WhiteListOrgT) "white_list_org" [("subscriberId", "subscriber_id")])

