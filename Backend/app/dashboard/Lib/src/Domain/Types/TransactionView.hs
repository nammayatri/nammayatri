{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Read-only view of the operator-action audit trail, without the app-typed
-- endpoint enum.
--
-- @Domain.Types.Transaction.Endpoint@ is a sum over the API action types of both
-- application packages, which pins it to lib-dashboard-api. The column itself is
-- @character varying@ holding that enum's Show form -- "PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_LIST"
-- -- so this view carries it as 'Text'.
--
-- That changes the JSON for this one field, and deliberately: lib-dashboard-api
-- encodes @Endpoint@ generically, as @{"tag": "ProviderManagementAPI", "contents": ...}@.
-- The frontend already accepts both -- its type is @TransactionEndpoint | string@
-- and @flattenEndpoint@ short-circuits on a string (control-center
-- src/lib/transactionLog.ts) -- and the string it flattens the tagged object INTO
-- is exactly the text stored here. So consumers see the value they were already
-- computing, one step earlier.
module Domain.Types.TransactionView
  ( TransactionRow (..),
    TransactionAPIEntity (..),
    RequestorAPIEntity (..),
    ListTransactionRes (..),
    mkTransactionAPIEntity,
  )
where

import Dashboard.Common (Summary)
import qualified Dashboard.Common.Driver as Common
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import Kernel.Types.Id

-- | One transaction row, endpoint left as the text the database holds.
data TransactionRow = TransactionRow
  { id :: Text,
    requestorId :: Maybe Text,
    merchantId :: Maybe Text,
    endpoint :: Text,
    commonDriverId :: Maybe Text,
    commonRideId :: Maybe Text,
    request :: Maybe Text,
    response :: Maybe Text,
    responseError :: Maybe Text,
    createdAt :: UTCTime
  }

data RequestorAPIEntity = RequestorAPIEntity
  { id :: Id DP.Person,
    firstName :: Text,
    lastName :: Text,
    email :: Maybe Text,
    mobileNumber :: Text,
    mobileCountryCode :: Text,
    registeredAt :: UTCTime,
    verified :: Maybe Bool
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data TransactionAPIEntity = TransactionAPIEntity
  { id :: Id TransactionRow,
    requestor :: RequestorAPIEntity,
    merchantId :: Maybe (Id DM.Merchant),
    endpoint :: Text,
    commonDriverId :: Maybe (Id Common.Driver),
    commonRideId :: Maybe (Id Common.Ride),
    request :: Maybe Text,
    response :: Maybe Text,
    responseError :: Maybe Text,
    createdAt :: UTCTime
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data ListTransactionRes = ListTransactionRes
  { list :: [TransactionAPIEntity],
    summary :: Summary
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

mkTransactionAPIEntity :: TransactionRow -> DP.DecryptedPerson -> TransactionAPIEntity
mkTransactionAPIEntity row requestor =
  TransactionAPIEntity
    { id = Id row.id,
      requestor = mkRequestorAPIEntity requestor,
      merchantId = Id <$> row.merchantId,
      endpoint = row.endpoint,
      commonDriverId = Id <$> row.commonDriverId,
      commonRideId = Id <$> row.commonRideId,
      request = row.request,
      response = row.response,
      responseError = row.responseError,
      createdAt = row.createdAt
    }
  where
    mkRequestorAPIEntity DP.Person {..} = RequestorAPIEntity {registeredAt = createdAt, ..}
