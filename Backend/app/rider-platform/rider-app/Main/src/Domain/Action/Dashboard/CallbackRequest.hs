{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Domain.Action.Dashboard.CallbackRequest where

import Data.Time hiding (getCurrentTime)
import qualified Domain.Types.CallbackRequest as DC
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Storage.Esqueleto hiding (count, isNothing)
import Kernel.Types.APISuccess
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.CallbackRequest as QCallbackRequest
import qualified Storage.Queries.Merchant as QMerchant

data CallbackRequestRes = CallbackRequestRes
  { list :: [DC.CallbackRequestAPIEntity],
    summary :: Summary
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data Summary = Summary
  { totalCount :: Int,
    count :: Int
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

getCallbackRequest :: ShortId DM.Merchant -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe DC.CallbackRequestStatus -> Flow CallbackRequestRes
getCallbackRequest merchantShortId mbLimit mbOffset mbMobileNumber mbCreatedAt mbStatus = do
  merchant <- runInReplica $ QMerchant.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  mbMobileNumberDbHash <- getDbHash `traverse` mbMobileNumber
  callbackRequest <- QCallbackRequest.fetchCallbackRequest merchant.id mbLimit mbOffset mbMobileNumberDbHash mbStatus mbCreatedAt
  let count = length callbackRequest
  callbackRequest' <- mapM DC.mkCallbackRequestAPIEntity callbackRequest
  let summary = Summary {totalCount = count, count}
  return $ CallbackRequestRes {list = callbackRequest', summary = summary}

updateCallbackRequestStatus :: (Monad m, EsqDBReplicaFlow m r, EsqDBFlow m r) => ShortId DM.Merchant -> Id DC.CallbackRequest -> DC.CallbackRequestStatus -> m APISuccess
updateCallbackRequestStatus merchantShortId id status = do
  merchant <- runInReplica $ QMerchant.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  runTransaction $ do
    QCallbackRequest.updateStatus merchant.id id status
  pure Success
