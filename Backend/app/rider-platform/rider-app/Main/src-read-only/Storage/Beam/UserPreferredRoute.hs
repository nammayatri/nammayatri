{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.UserPreferredRoute where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Database.Beam as B



data UserPreferredRouteT f
    = UserPreferredRouteT {createdAt :: (B.C f Kernel.Prelude.UTCTime),
                           fromArea :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                           fromAreaCode :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                           fromBuilding :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                           fromCity :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                           fromCountry :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                           fromDoor :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                           fromExtras :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                           fromInstructions :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                           fromPlaceId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                           fromState :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                           fromStreet :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                           fromTitle :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                           fromWard :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                           fromLocationLat :: (B.C f Kernel.Prelude.Double),
                           fromLocationLon :: (B.C f Kernel.Prelude.Double),
                           id :: (B.C f Kernel.Prelude.Text),
                           personId :: (B.C f Kernel.Prelude.Text),
                           priority :: (B.C f Kernel.Prelude.Int),
                           routeName :: (B.C f Kernel.Prelude.Text),
                           toArea :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                           toAreaCode :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                           toBuilding :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                           toCity :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                           toCountry :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                           toDoor :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                           toExtras :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                           toInstructions :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                           toPlaceId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                           toState :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                           toStreet :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                           toTitle :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                           toWard :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                           toLocationLat :: (B.C f Kernel.Prelude.Double),
                           toLocationLon :: (B.C f Kernel.Prelude.Double),
                           updatedAt :: (B.C f Kernel.Prelude.UTCTime),
                           usageCount :: (B.C f Kernel.Prelude.Int)}
    deriving (Generic, B.Beamable)
instance B.Table UserPreferredRouteT
    where data PrimaryKey UserPreferredRouteT f = UserPreferredRouteId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = UserPreferredRouteId . id
type UserPreferredRoute = UserPreferredRouteT Identity

$(enableKVPG (''UserPreferredRouteT) [('id)] [[('personId)]])

$(mkTableInstances (''UserPreferredRouteT) "user_preferred_route")

