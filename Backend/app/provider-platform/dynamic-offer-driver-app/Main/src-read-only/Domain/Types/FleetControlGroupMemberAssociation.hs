{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FleetControlGroupMemberAssociation where

import Data.Aeson
import qualified Domain.Types.FleetControlGroup
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data FleetControlGroupMemberAssociation = FleetControlGroupMemberAssociation
  { fleetControlGroupId :: Kernel.Types.Id.Id Domain.Types.FleetControlGroup.FleetControlGroup,
    fleetMemberId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
