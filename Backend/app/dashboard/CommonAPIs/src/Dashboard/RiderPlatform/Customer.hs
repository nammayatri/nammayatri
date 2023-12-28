{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}

module Dashboard.RiderPlatform.Customer
  ( module Dashboard.RiderPlatform.Customer,
    module Reexport,
  )
where

import Dashboard.Common as Reexport
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Common (HighPrecMoney)
import Kernel.Types.Id
import Servant hiding (Summary)

data CustomerEndpoint
  = DeleteCustomerEndpoint
  | BlockCustomerEndpoint
  | UnblockCustomerEndpoint
  | CustomerCancellationDuesSyncEndpoint
  deriving (Show, Read)

derivePersistField "CustomerEndpoint"

---------------------------------------------------------
-- customer delete --------------------------------------

type CustomerDeleteAPI =
  Capture "customerId" (Id Customer)
    :> "delete"
    :> Post '[JSON] APISuccess

---------------------------------------------------------
-- customer block  --------------------------------------

type CustomerBlockAPI =
  Capture "customerId" (Id Customer)
    :> "block"
    :> Post '[JSON] APISuccess

---------------------------------------------------------
-- customer unblock  ------------------------------------

type CustomerUnblockAPI =
  Capture "customerId" (Id Customer)
    :> "unblock"
    :> Post '[JSON] APISuccess

---------------------------------------------------------
-- customer info  ---------------------------------------

-- Do we need to use filters instead of customerId, like in driverInfo API?
-- {{bpp-dashboard-host}}/bpp/driver-offer/NAMMA_YATRI_PARTNER/driver/info?mobileNumber=6666666666

type CustomerInfoAPI =
  Capture "customerId" (Id Customer)
    :> "info"
    :> Get '[JSON] CustomerInfoRes

newtype CustomerInfoRes = CustomerInfoRes
  { numberOfRides :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

---------------------------------------------------------
-- customer List ----------------------------------------

type CustomerListAPI =
  "list"
    :> QueryParam "limit" Int
    :> QueryParam "offset" Int
    :> QueryParam "enabled" Bool
    :> QueryParam "blocked" Bool
    :> QueryParam "phone" Text
    :> Get '[JSON] CustomerListRes

data CustomerListRes = CustomerListRes
  { totalItems :: Int, -- for backward compatibility
    summary :: Summary,
    customers :: [CustomerListItem]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CustomerListItem = CustomerListItem
  { customerId :: Id Customer,
    firstName :: Maybe Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    phoneNo :: Maybe Text,
    enabled :: Bool,
    blocked :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

---------------------------------------------------------------------------
-- customer cancellation dues sync ----------------------------------------

type CustomerCancellationDuesSyncAPI =
  Capture "customerId" (Id Customer)
    :> "cancellationDuesSync"
    :> ReqBody '[JSON] CustomerCancellationDuesSyncReq
    :> Post '[JSON] APISuccess

instance HideSecrets CustomerCancellationDuesSyncReq where
  hideSecrets = identity

data CustomerCancellationDuesSyncReq = CustomerCancellationDuesSyncReq
  { cancellationCharges :: Maybe HighPrecMoney,
    disputeChancesUsed :: Maybe Int,
    paymentMadeToDriver :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

---------------------------------------------------------------------------
-- get customer cancellation dues details ----------------------------------------

type GetCancellationDuesDetailsAPI =
  Capture "customerId" (Id Customer)
    :> "getCancellationDuesDetails"
    :> Get '[JSON] CancellationDuesDetailsRes

data CancellationDuesDetailsRes = CancellationDuesDetailsRes
  { cancellationDues :: HighPrecMoney,
    disputeChancesUsed :: Int,
    canBlockCustomer :: Maybe Bool
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
