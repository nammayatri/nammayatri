{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.SearchRequestSpecialZone where

import qualified Domain.Types.LocationMapping as LM
import Domain.Types.Merchant
import Domain.Types.SearchRequestSpecialZone as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Queries.FullEntityBuilders
import qualified Storage.Queries.LocationMapping as QLocationMapping
import Storage.Tabular.SearchRequestSpecialZone

create :: SearchRequestSpecialZone -> SqlDB ()
create dsReq = do
  mappings <- LM.locationMappingMaker dsReq LM.SearchRequest
  Esq.runTransaction $
    withFullEntity dsReq $ \(sReq, fromLoc, toLoc) -> do
      Esq.create' sReq
      void $ Esq.createUnique' fromLoc
      traverse_ Esq.createUnique' toLoc
  QLocationMapping.createMany mappings

findById ::
  (Transactionable m) =>
  Id SearchRequestSpecialZone ->
  m (Maybe SearchRequestSpecialZone)
findById searchRequestId = Esq.buildDType $ do
  res <- findOne' $ do
    searchRequestT <- from $ table @SearchRequestSpecialZoneT
    where_ $
      searchRequestT ^. SearchRequestSpecialZoneId ==. val (getId searchRequestId)
    return searchRequestT
  join <$> mapM buildFullSearchRequestSpecialZone res

findByTransactionId ::
  (Transactionable m) =>
  Id SearchRequestSpecialZone ->
  m (Maybe (Id SearchRequestSpecialZone))
findByTransactionId tId = do
  findOne $ do
    searchT <- from $ table @SearchRequestSpecialZoneT
    where_ $
      searchT ^. SearchRequestSpecialZoneTransactionId ==. val (getId tId)
    return $ searchT ^. SearchRequestSpecialZoneTId

findByMsgIdAndBapIdAndBppId :: Transactionable m => Text -> Text -> Id Merchant -> m (Maybe SearchRequestSpecialZone)
findByMsgIdAndBapIdAndBppId txnId bapId merchantId = Esq.buildDType $ do
  mbFullSearchReqT <- Esq.findOne' $ do
    sReq <- from $ table @SearchRequestSpecialZoneT
    where_ $
      sReq ^. SearchRequestSpecialZoneMessageId ==. val txnId
        &&. sReq ^. SearchRequestSpecialZoneProviderId ==. val (toKey merchantId)
        &&. sReq ^. SearchRequestSpecialZoneBapId ==. val bapId
    pure sReq
  join <$> mapM buildFullSearchRequestSpecialZone mbFullSearchReqT

getValidTill ::
  (Transactionable m) =>
  Id SearchRequestSpecialZone ->
  m (Maybe UTCTime)
getValidTill searchRequestId = do
  findOne $ do
    searchT <- from $ table @SearchRequestSpecialZoneT
    where_ $
      searchT ^. SearchRequestSpecialZoneTId ==. val (toKey searchRequestId)
    return $ searchT ^. SearchRequestSpecialZoneValidTill
