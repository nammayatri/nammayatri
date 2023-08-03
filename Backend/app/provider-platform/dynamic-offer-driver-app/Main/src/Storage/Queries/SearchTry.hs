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
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.SearchTry as BeamST

create :: (L.MonadFlow m, Log m) => SearchTry -> m ()
create = createWithKV

findById :: (L.MonadFlow m, Log m) => Id SearchTry -> m (Maybe SearchTry)
findById (Id searchTry) = findOneWithKV [Se.Is BeamST.id $ Se.Eq searchTry]

findLastByRequestId ::
  (L.MonadFlow m, Log m) =>
  Id SearchRequest ->
  m (Maybe SearchTry)
findLastByRequestId (Id searchRequest) = findAllWithOptionsKV [Se.Is BeamST.id $ Se.Eq searchRequest] (Se.Desc BeamST.searchRepeatCounter) (Just 1) Nothing <&> listToMaybe

findActiveTryByRequestId ::
  (L.MonadFlow m, Log m) =>
  Id SearchRequest ->
  m (Maybe SearchTry)
findActiveTryByRequestId (Id searchRequest) = findAllWithOptionsKV [Se.And [Se.Is BeamST.id $ Se.Eq searchRequest, Se.Is BeamST.status $ Se.Eq ACTIVE]] (Se.Desc BeamST.searchRepeatCounter) (Just 1) Nothing <&> listToMaybe

cancelActiveTriesByRequestId ::
  (L.MonadFlow m, MonadTime m, Log m) =>
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
  (L.MonadFlow m, MonadTime m, Log m) =>
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
  (L.MonadFlow m, Log m) =>
  Id SearchTry ->
  m (Maybe (UTCTime, SearchTryStatus))
getSearchTryStatusAndValidTill (Id searchTryId) = findOneWithKV [Se.Is BeamST.id $ Se.Eq searchTryId] <&> fmap (\st -> (Domain.validTill st, Domain.status st))

instance FromTType' BeamST.SearchTry SearchTry where
  fromTType' BeamST.SearchTryT {..} = do
    pure $
      Just
        SearchTry
          { id = Id id,
            requestId = Id requestId,
            estimateId = Id estimateId,
            merchantId = Id <$> merchantId,
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
