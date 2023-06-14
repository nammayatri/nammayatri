{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.CallbackRequest where

import Domain.Types.CallbackRequest
import Domain.Types.Merchant as DM
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Types.Time
import Storage.Tabular.CallbackRequest

create :: CallbackRequest -> SqlDB ()
create = Esq.create

fetchCallbackRequest :: Transactionable m => Id DM.Merchant -> Maybe Int -> Maybe Int -> Maybe DbHash -> Maybe CallbackRequestStatus -> Maybe UTCTime -> m [CallbackRequest]
fetchCallbackRequest merchantId mbLimit mbOffset mbMobileNumberDbHash mbStatus mbCreatedAt = do
  Esq.findAll $ do
    callbackrequest <- from $ table @CallbackRequestT
    where_ $
      callbackrequest ^. CallbackRequestMerchantId ==. val (toKey merchantId)
        &&. whenJust_ mbMobileNumberDbHash (\mobileNumberDbHash -> callbackrequest ^. CallbackRequestCustomerPhoneHash ==. val mobileNumberDbHash)
        &&. whenJust_ mbStatus (\status -> callbackrequest ^. CallbackRequestStatus ==. val status)
        &&. whenJust_ mbCreatedAt (\createdAt -> callbackrequest ^. CallbackRequestCreatedAt >=. val createdAt)
    orderBy [desc $ callbackrequest ^. CallbackRequestCreatedAt]
    limit limitVal
    offset offsetVal
    pure callbackrequest
  where
    limitVal = min (maybe 10 fromIntegral mbLimit) 10
    offsetVal = maybe 0 fromIntegral mbOffset

updateStatus :: Id DM.Merchant -> Id CallbackRequest -> CallbackRequestStatus -> SqlDB ()
updateStatus merchantId id status = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ CallbackRequestStatus =. val status,
        CallbackRequestUpdatedAt =. val now
      ]
    where_ $ tbl ^. CallbackRequestTId ==. val (toKey id) &&. tbl ^. CallbackRequestMerchantId ==. val (toKey merchantId)
