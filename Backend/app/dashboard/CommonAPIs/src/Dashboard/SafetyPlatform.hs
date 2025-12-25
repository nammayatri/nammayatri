{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}

module Dashboard.SafetyPlatform where

--- this  file is created to declare the endpoints for transaction as we can't to in safety dashboard it will create cyclic dependency with lib-dashboard
import Kernel.Prelude
import Kernel.Storage.Esqueleto

data SafetyEndpoint
  = ChangeSuspectFlagEndpoint
  | AddFlagCategoryEndpoint
  | DeleteFlagCategoryEndpoint
  | SetMerchantConfigEndpoint
  | UploadSuspectEndpoint
  | UploadBulkSuspectEndpoint
  | ChangeFlagRequestEndpoint
  | ProcessSuspectFlagRequestEndpoint
  | ProcessSuspectFlagRequestListEndpoint
  | ResetPasswordEndpoint
  | UpdateReceiveNotificationStatusEndpoint
  deriving (Show, Read, ToJSON, FromJSON, Generic, Eq, Ord, ToSchema)

derivePersistField "SafetyEndpoint"
