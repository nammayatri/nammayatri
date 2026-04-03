{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.FleetBadgeAssociation where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Id
import qualified Domain.Types.FleetBadge
import qualified Domain.Types.FleetBadgeType
import qualified Domain.Types.Person
import qualified Tools.Beam.UtilsTH



data FleetBadgeAssociation
    = FleetBadgeAssociation {associatedOn :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
                             associatedTill :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
                             badgeId :: Kernel.Types.Id.Id Domain.Types.FleetBadge.FleetBadge,
                             badgeType :: Domain.Types.FleetBadgeType.FleetBadgeType,
                             createdAt :: Kernel.Prelude.UTCTime,
                             driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
                             fleetOwnerId :: Kernel.Prelude.Text,
                             id :: Kernel.Types.Id.Id Domain.Types.FleetBadgeAssociation.FleetBadgeAssociation,
                             isActive :: Kernel.Prelude.Bool,
                             updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



