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
import Kernel.ServantMultipart
import Kernel.Storage.Esqueleto
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Common (PriceAPIEntity)
import Kernel.Types.Id
import Servant hiding (Summary)

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

---------------------------------------------------------------------------
-- customer cancellation dues sync ----------------------------------------

-- instance HideSecrets CustomerCancellationDuesSyncReq where
--   hideSecrets = identity

-------------------------------------------------------------------------
-- get customer cancellation dues details ----------------------------------------

type GetCancellationDuesDetailsAPI =
  Capture "customerId" (Id Customer)
    :> "getCancellationDuesDetails"
    :> Get '[JSON] CancellationDuesDetailsRes

data CancellationDuesDetailsRes = CancellationDuesDetailsRes
  { cancellationDues :: Maybe PriceAPIEntity,
    disputeChancesUsed :: Maybe Int,
    canBlockCustomer :: Maybe Bool
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-----------------------------------------------------------------------------
-- safety center blocking api ----------------------------------------------

type UpdateSafetyCenterBlockingAPI =
  Capture "customerId" (Id Customer)
    :> "updateSafetyCenterBlocking"
    :> ReqBody '[JSON] UpdateSafetyCenterBlockingReq
    :> Post '[JSON] APISuccess

data UpdateSafetyCenterBlockingReq = UpdateSafetyCenterBlockingReq
  { incrementCount :: Maybe Bool,
    resetCount :: Maybe Bool
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

type PostCustomersPersonNumbersAPI =
  ( "personNumbers" :> Kernel.ServantMultipart.MultipartForm Kernel.ServantMultipart.Tmp PersonIdsReq
      :> Post '[JSON] [PersonRes]
  )

type PostDriverPersonIdAPI =
  ( "personId" :> Kernel.ServantMultipart.MultipartForm Kernel.ServantMultipart.Tmp PersonMobileNoReq
      :> Post '[JSON] [PersonRes]
  )
