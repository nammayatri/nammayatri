{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.BlackListOrg where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Types.Beckn.Domain
import qualified Kernel.Prelude
import qualified Database.Beam as B



data BlackListOrgT f
    = BlackListOrgT {domain :: (B.C f Kernel.Types.Beckn.Domain.Domain),
                     id :: (B.C f Kernel.Prelude.Text),
                     merchantId :: (B.C f Kernel.Prelude.Text),
                     merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
                     subscriberId :: (B.C f Kernel.Prelude.Text)}
    deriving (Generic, B.Beamable)
instance B.Table BlackListOrgT
    where data PrimaryKey BlackListOrgT f = BlackListOrgId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = BlackListOrgId . id
type BlackListOrg = BlackListOrgT Identity

$(enableKVPG (''BlackListOrgT) [('id)] [[('subscriberId)]])

$(mkTableInstancesWithTModifier (''BlackListOrgT) "black_list_org" [("subscriberId", "subscriber_id")])

