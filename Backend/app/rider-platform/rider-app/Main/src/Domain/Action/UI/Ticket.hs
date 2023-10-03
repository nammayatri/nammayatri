{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Ticket where

-- import qualified Domain.Types.Quote as DQuote

import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ticket as DTT
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Ticket as QRT
import Tools.Error

ticketList :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id DTT.Ticket -> (Id Person.Person, Id Merchant.Merchant) -> m DTT.TicketAPIEntity
ticketList ticketId (_, _) = do
  ticket <- runInReplica (QRT.findById ticketId) >>= fromMaybeM (TicketDoesNotExist ticketId.getId)
  qrData <- toQRData ticket ticketId
  logInfo $ "ticket: " <> show ticket
  pure $
    DTT.TicketAPIEntity
      { id = ticket.id,
        status = ticket.status,
        quoteId = ticket.quoteId,
        paymentUrl = ticket.paymentUrl,
        quantity = ticket.quantity,
        pricePerAdult = ticket.pricePerAdult,
        totalPrice = ticket.totalPrice,
        ..
      }

toQRData :: MonadFlow m => DTT.Ticket -> Id DTT.Ticket -> m Text
toQRData ticket ticketId = if ticket.status == DTT.CONFIRMED then ticket.qrData & fromMaybeM (TicketFieldNotFound $ "qrData: " <> ticketId.getId) else pure ""
