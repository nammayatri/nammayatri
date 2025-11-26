{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.FleetOwnerList where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.FleetOwnerInformation
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

data FleetOwnerListItem = FleetOwnerListItem
  { fleetOwnerId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    fleetOwnerName :: Kernel.Prelude.Text,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    fleetType :: Domain.Types.FleetOwnerInformation.FleetType,
    enabled :: Kernel.Prelude.Bool,
    verified :: Kernel.Prelude.Bool,
    blocked :: Kernel.Prelude.Bool,
    isEligibleForSubscription :: Kernel.Prelude.Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
