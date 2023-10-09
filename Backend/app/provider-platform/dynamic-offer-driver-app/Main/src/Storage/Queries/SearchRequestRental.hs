{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.SearchRequestRental where

import Domain.Types.Merchant
import Domain.Types.SearchRequestRental as Domain
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Error
import qualified Sequelize as Se
import qualified Storage.Beam.SearchRequestRental as BeamSRR
import Storage.Queries.SearchRequest.SearchReqLocation as QSRL

createSearchRequestRental :: MonadFlow m => SearchRequestRental -> m ()
createSearchRequestRental = createWithKV

create :: MonadFlow m => SearchRequestRental -> m ()
create srr = QSRL.create srr.fromLocation >> QSRL.create srr.toLocation >> createSearchRequestRental srr

findById :: MonadFlow m => Id SearchRequestRental -> m (Maybe SearchRequestRental)
findById (Id searchRequestRentalId) = findOneWithKV [Se.Is BeamSRR.id $ Se.Eq searchRequestRentalId]

getRequestIdfromTransactionId :: MonadFlow m => Id SearchRequestRental -> m (Maybe (Id SearchRequestRental))
getRequestIdfromTransactionId (Id tId) = findOneWithKV [Se.Is BeamSRR.transactionId $ Se.Eq tId] <&> (Domain.id <$>)

findByMsgIdAndBapIdAndBppId :: MonadFlow m => Text -> Text -> Id Merchant -> m (Maybe SearchRequestRental)
findByMsgIdAndBapIdAndBppId txnId bapId (Id merchantId) = findOneWithKV [Se.And [Se.Is BeamSRR.messageId $ Se.Eq txnId, Se.Is BeamSRR.providerId $ Se.Eq merchantId, Se.Is BeamSRR.bapId $ Se.Eq bapId]]

findByTransactionId ::
  MonadFlow m =>
  Id SearchRequestRental ->
  m (Maybe (Id SearchRequestRental))
findByTransactionId (Id tId) = findOneWithKV [Se.Is BeamSRR.transactionId $ Se.Eq tId] <&> (Domain.id <$>)

getValidTill :: MonadFlow m => Id SearchRequestRental -> m (Maybe UTCTime)
getValidTill (Id searchRequestId) = do
  findOneWithKV [Se.Is BeamSRR.id $ Se.Eq searchRequestId] <&> (Domain.validTill <$>)

instance FromTType' BeamSRR.SearchRequestRental SearchRequestRental where
  fromTType' BeamSRR.SearchRequestRentalT {..} = do
    fl <- QSRL.findById (Id fromLocationId) >>= fromMaybeM (InternalError $ "FromLocation not found in SearchRequestRental for fromLocationId: " <> show fromLocationId)
    pUrl <- parseBaseUrl bapUri
    pure $
      Just
        SearchRequestRental
          { id = Id id,
            transactionId = transactionId,
            messageId = messageId,
            startTime = startTime,
            validTill = validTill,
            providerId = Id providerId,
            fromLocation = fl,
            area = area,
            bapId = bapId,
            bapUri = pUrl,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' BeamSRR.SearchRequestRental SearchRequestRental where
  toTType' SearchRequestRental {..} = do
    BeamSRR.SearchRequestRentalT
      { BeamSRR.id = getId id,
        BeamSRR.transactionId = transactionId,
        BeamSRR.messageId = messageId,
        BeamSRR.startTime = startTime,
        BeamSRR.validTill = validTill,
        BeamSRR.providerId = getId providerId,
        BeamSRR.fromLocationId = getId fromLocation.id,
        BeamSRR.area = area,
        BeamSRR.bapId = bapId,
        BeamSRR.bapUri = showBaseUrl bapUri,
        BeamSRR.createdAt = createdAt,
        BeamSRR.updatedAt = updatedAt
      }
