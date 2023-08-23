{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.SearchRequestSpecialZone where

import Domain.Types.Merchant
import Domain.Types.SearchRequestSpecialZone as Domain
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Error
import qualified Sequelize as Se
import qualified Storage.Beam.SearchRequestSpecialZone as BeamSRSZ
import Storage.Queries.SearchRequest.SearchReqLocation as QSRL

createSearchRequestSpecialZone :: MonadFlow m => SearchRequestSpecialZone -> m ()
createSearchRequestSpecialZone = createWithKV

create :: MonadFlow m => SearchRequestSpecialZone -> m ()
create srsz = QSRL.create srsz.fromLocation >> QSRL.create srsz.toLocation >> createSearchRequestSpecialZone srsz

findById :: MonadFlow m => Id SearchRequestSpecialZone -> m (Maybe SearchRequestSpecialZone)
findById (Id searchRequestSpecialZoneId) = findOneWithKV [Se.Is BeamSRSZ.id $ Se.Eq searchRequestSpecialZoneId]

getRequestIdfromTransactionId :: MonadFlow m => Id SearchRequestSpecialZone -> m (Maybe (Id SearchRequestSpecialZone))
getRequestIdfromTransactionId (Id tId) = findOneWithKV [Se.Is BeamSRSZ.transactionId $ Se.Eq tId] <&> (Domain.id <$>)

findByTxnIdAndBapIdAndBppId :: MonadFlow m => Text -> Text -> Id Merchant -> m (Maybe SearchRequestSpecialZone)
findByTxnIdAndBapIdAndBppId txnId bapId (Id merchantId) = findOneWithKV [Se.And [Se.Is BeamSRSZ.messageId $ Se.Eq txnId, Se.Is BeamSRSZ.providerId $ Se.Eq merchantId, Se.Is BeamSRSZ.bapId $ Se.Eq bapId]]

findByTransactionId ::
  MonadFlow m =>
  Id SearchRequestSpecialZone ->
  m (Maybe (Id SearchRequestSpecialZone))
findByTransactionId (Id tId) = findOneWithKV [Se.Is BeamSRSZ.transactionId $ Se.Eq tId] <&> (Domain.id <$>)

getValidTill :: MonadFlow m => Id SearchRequestSpecialZone -> m (Maybe UTCTime)
getValidTill (Id searchRequestId) = do
  findOneWithKV [Se.Is BeamSRSZ.id $ Se.Eq searchRequestId] <&> (Domain.validTill <$>)

instance FromTType' BeamSRSZ.SearchRequestSpecialZone SearchRequestSpecialZone where
  fromTType' BeamSRSZ.SearchRequestSpecialZoneT {..} = do
    fl <- QSRL.findById (Id fromLocationId) >>= fromMaybeM (InternalError $ "FromLocation not found in SearchRequestSpecialZone for fromLocationId: " <> show fromLocationId)
    tl <- QSRL.findById (Id toLocationId) >>= fromMaybeM (InternalError $ "ToLocation not found in SearchRequestSpecialZone for toLocationId: " <> show toLocationId)
    pUrl <- parseBaseUrl bapUri
    pure $
      Just
        SearchRequestSpecialZone
          { id = Id id,
            transactionId = transactionId,
            messageId = messageId,
            startTime = startTime,
            validTill = validTill,
            providerId = Id providerId,
            fromLocation = fl,
            toLocation = tl,
            area = area,
            bapId = bapId,
            bapUri = pUrl,
            estimatedDistance = estimatedDistance,
            estimatedDuration = estimatedDuration,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' BeamSRSZ.SearchRequestSpecialZone SearchRequestSpecialZone where
  toTType' SearchRequestSpecialZone {..} = do
    BeamSRSZ.SearchRequestSpecialZoneT
      { BeamSRSZ.id = getId id,
        BeamSRSZ.transactionId = transactionId,
        BeamSRSZ.messageId = messageId,
        BeamSRSZ.startTime = startTime,
        BeamSRSZ.validTill = validTill,
        BeamSRSZ.providerId = getId providerId,
        BeamSRSZ.fromLocationId = getId fromLocation.id,
        BeamSRSZ.toLocationId = getId toLocation.id,
        BeamSRSZ.area = area,
        BeamSRSZ.bapId = bapId,
        BeamSRSZ.bapUri = showBaseUrl bapUri,
        BeamSRSZ.estimatedDistance = estimatedDistance,
        BeamSRSZ.estimatedDuration = estimatedDuration,
        BeamSRSZ.createdAt = createdAt,
        BeamSRSZ.updatedAt = updatedAt
      }
