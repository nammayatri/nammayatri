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

data CustomerEndpoint
  = DeleteCustomerEndpoint
  | BlockCustomerEndpoint
  | UnblockCustomerEndpoint
  | CustomerCancellationDuesSyncEndpoint
  | UpdateSafetyCenterEndpoint
  | PostCustomerPersonNumbersEndpoint
  | PostDriverpersonIdEndpoint
  deriving (Show, Read, ToJSON, FromJSON, Generic, Eq, Ord, ToSchema)

derivePersistField "CustomerEndpoint"

-- Do we need to use filters instead of customerId, like in driverInfo API?
-- {{bpp-dashboard-host}}/bpp/driver-offer/NAMMA_YATRI_PARTNER/driver/info?mobileNumber=6666666666
