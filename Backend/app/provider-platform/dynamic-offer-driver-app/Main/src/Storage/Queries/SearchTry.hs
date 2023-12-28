{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.SearchTry where

import qualified Database.Beam.Query ()
import Domain.Types.SearchRequest (SearchRequest)
import Domain.Types.SearchTry as Domain
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.SearchTry as BeamST
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.SearchRequest as QR

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => SearchTry -> m ()
create = createWithKV

findById :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id SearchTry -> m (Maybe SearchTry)
findById (Id searchTry) = findOneWithKV [Se.Is BeamST.id $ Se.Eq searchTry]

findLastByRequestId ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id SearchRequest ->
  m (Maybe SearchTry)
findLastByRequestId (Id searchRequest) = findAllWithOptionsKV [Se.Is BeamST.requestId $ Se.Eq searchRequest] (Se.Desc BeamST.searchRepeatCounter) (Just 1) Nothing <&> listToMaybe

findTryByRequestId ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id SearchRequest ->
  m (Maybe SearchTry)
findTryByRequestId (Id searchRequest) =
  findAllWithOptionsKV
    [Se.Is BeamST.requestId $ Se.Eq searchRequest]
    (Se.Desc BeamST.searchRepeatCounter)
    (Just 1)
    Nothing
    <&> listToMaybe

cancelActiveTriesByRequestId ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id SearchRequest ->
  m ()
cancelActiveTriesByRequestId (Id searchId) = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamST.status CANCELLED,
      Se.Set BeamST.updatedAt now
    ]
    [ Se.And
        [ Se.Is BeamST.requestId $ Se.Eq searchId,
          Se.Is BeamST.status $ Se.Eq ACTIVE
        ]
    ]

updateStatus ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id SearchTry ->
  SearchTryStatus ->
  m ()
updateStatus (Id searchId) status_ = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamST.status status_,
      Se.Set BeamST.updatedAt now
    ]
    [Se.Is BeamST.id $ Se.Eq searchId]

getSearchTryStatusAndValidTill ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id SearchTry ->
  m (Maybe (UTCTime, SearchTryStatus))
getSearchTryStatusAndValidTill (Id searchTryId) = findOneWithKV [Se.Is BeamST.id $ Se.Eq searchTryId] <&> fmap (\st -> (Domain.validTill st, Domain.status st))

instance FromTType' BeamST.SearchTry SearchTry where
  fromTType' BeamST.SearchTryT {..} = do
    merchant <- case merchantId of
      Nothing -> do
        searchReq <- QR.findById (Id requestId) >>= fromMaybeM (InternalError $ "Search request not found - " <> requestId)
        CQM.findById searchReq.providerId >>= fromMaybeM (MerchantNotFound searchReq.providerId.getId)
      Just mId -> CQM.findById (Id mId) >>= fromMaybeM (MerchantNotFound mId)
    merchantOpCityId <- CQMOC.getMerchantOpCityId (Id <$> merchantOperatingCityId) merchant Nothing
    pure $
      Just
        SearchTry
          { id = Id id,
            requestId = Id requestId,
            estimateId = Id estimateId,
            merchantId = Id <$> merchantId,
            merchantOperatingCityId = merchantOpCityId,
            messageId = messageId,
            startTime = startTime,
            validTill = validTill,
            vehicleVariant = vehicleVariant,
            baseFare = baseFare,
            customerExtraFee = customerExtraFee,
            status = status,
            searchRepeatCounter = searchRepeatCounter,
            searchRepeatType = searchRepeatType,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' BeamST.SearchTry SearchTry where
  toTType' SearchTry {..} = do
    BeamST.SearchTryT
      { id = getId id,
        requestId = getId requestId,
        estimateId = getId estimateId,
        merchantId = getId <$> merchantId,
        merchantOperatingCityId = Just $ getId merchantOperatingCityId,
        messageId = messageId,
        startTime = startTime,
        validTill = validTill,
        vehicleVariant = vehicleVariant,
        baseFare = baseFare,
        customerExtraFee = customerExtraFee,
        status = status,
        searchRepeatCounter = searchRepeatCounter,
        searchRepeatType = searchRepeatType,
        createdAt = createdAt,
        updatedAt = updatedAt
      }
