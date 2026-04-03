{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.FleetBadge where
import Kernel.Prelude
import Data.Aeson
import qualified Domain.Types.FleetBadgeType
import qualified Kernel.Types.Id
import qualified Domain.Types.Person
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Tools.Beam.UtilsTH



data FleetBadge
    = FleetBadge {badgeName :: Kernel.Prelude.Text,
                  badgeRank :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                  badgeType :: Domain.Types.FleetBadgeType.FleetBadgeType,
                  createdAt :: Kernel.Prelude.UTCTime,
                  fleetOwnerId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
                  id :: Kernel.Types.Id.Id Domain.Types.FleetBadge.FleetBadge,
                  merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                  merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
                  personId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
                  updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



