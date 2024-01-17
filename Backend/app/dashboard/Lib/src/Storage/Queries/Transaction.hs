{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Transaction where

import Domain.Types.ServerName as DSN
import Domain.Types.Transaction as DT
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import qualified Storage.Beam.Transaction as BeamT
import Storage.Tabular.Transaction

create :: Transaction -> SqlDB ()
create = Esq.create

fetchLastTransaction :: Transactionable m => DT.Endpoint -> DSN.ServerName -> m (Maybe DT.Transaction)
fetchLastTransaction endpoint serverName = do
  findOne $ do
    transaction <- from $ table @TransactionT
    where_ $
      transaction ^. TransactionEndpoint ==. val endpoint
        &&. transaction ^. TransactionServerName ==. val (Just serverName)
    orderBy [desc $ transaction ^. TransactionCreatedAt]
    limit 1
    return transaction

instance FromTType' BeamT.Transaction DT.Transaction where
  fromTType' BeamT.TransactionT {..} = do
    pure $
      Just
        DT.Transaction
          { id = Id id,
            requestorId = Id <$> requestorId,
            merchantId = Id <$> merchantId,
            commonDriverId = Id <$> commonDriverId,
            commonRideId = Id <$> commonRideId,
            ..
          }

instance ToTType' BeamT.Transaction DT.Transaction where
  toTType' DT.Transaction {..} =
    BeamT.TransactionT
      { id = getId id,
        requestorId = getId <$> requestorId,
        merchantId = getId <$> merchantId,
        commonDriverId = getId <$> commonDriverId,
        commonRideId = getId <$> commonRideId,
        ..
      }
