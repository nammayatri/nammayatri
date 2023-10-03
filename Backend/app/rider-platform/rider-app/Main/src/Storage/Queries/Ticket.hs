{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Ticket where

import Domain.Types.Ticket
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Error.Throwing
import qualified Sequelize as Se
import qualified Storage.Beam.Ticket as BeamT
import qualified Storage.Queries.Booking.BookingLocation as QBBL

createTicket :: MonadFlow m => Ticket -> m ()
createTicket = createWithKV

create :: MonadFlow m => Ticket -> m ()
create dTicket = do
  void $ createTicket dTicket

findAll :: MonadFlow m => m [Ticket]
findAll = findAllWithKV [Se.Is BeamT.id $ Se.Not $ Se.Eq ""]

findById :: MonadFlow m => Id Ticket -> m (Maybe Ticket)
findById (Id ticketId) = findOneWithKV [Se.Is BeamT.id $ Se.Eq ticketId]

updateStatus :: MonadFlow m => Id Ticket -> TicketStatus -> m ()
updateStatus rtId rtStatus = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamT.status rtStatus,
      Se.Set BeamT.updatedAt now
    ]
    [Se.Is BeamT.id (Se.Eq $ getId rtId)]

updateBPPTicketId :: MonadFlow m => Id Ticket -> Id BPPTicket -> m ()
updateBPPTicketId rbId bppRbId = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamT.bppTicketId (Just $ getId bppRbId),
      Se.Set BeamT.updatedAt now
    ]
    [Se.Is BeamT.id (Se.Eq $ getId rbId)]

findByBPPTicketId :: MonadFlow m => Id BPPTicket -> m (Maybe Ticket)
findByBPPTicketId (Id bppRbId) = findOneWithKV [Se.Is BeamT.bppTicketId $ Se.Eq $ Just bppRbId]

findByTransactionId :: MonadFlow m => Text -> m (Maybe Ticket)
findByTransactionId transactionId = findOneWithKV [Se.Is BeamT.search_request_id $ Se.Eq transactionId]

instance ToTType' BeamT.Ticket Ticket where
  toTType' Ticket {..} = do
    BeamT.TicketT
      { BeamT.id = getId id,
        BeamT.status = status,
        BeamT.quote_id = getId <$> quoteId,
        BeamT.itemId = itemId,
        BeamT.fulfillmentId = fulfillmentId,
        BeamT.paymentUrl = paymentUrl,
        BeamT.search_request_id = searchRequestId,
        BeamT.bpp_order_id = bppOrderId,
        BeamT.quantity = quantity,
        BeamT.fromLocationId = getId fromLocation.id,
        BeamT.price_per_adult = pricePerAdult,
        BeamT.total_price = totalPrice,
        BeamT.qr_data = qrData,
        BeamT.merchantId = getId merchantId,
        BeamT.providerId = providerId,
        BeamT.providerUrl = showBaseUrl providerUrl,
        BeamT.bppTicketId = getId <$> bppTicketId,
        BeamT.createdAt = createdAt,
        BeamT.updatedAt = updatedAt
      }

instance FromTType' BeamT.Ticket Ticket where
  fromTType' BeamT.TicketT {..} = do
    pUrl <- parseBaseUrl providerUrl
    fl <- QBBL.findById (Id fromLocationId) >>= fromMaybeM (InternalError $ "fromLocation not found in ticket for fromLocationId: " <> show fromLocationId)
    pure $
      Just
        Ticket
          { id = Id id,
            quoteId = Id <$> quote_id,
            searchRequestId = search_request_id,
            bppOrderId = bpp_order_id,
            pricePerAdult = price_per_adult,
            totalPrice = total_price,
            qrData = qr_data,
            merchantId = Id merchantId,
            providerId = providerId,
            providerUrl = pUrl,
            itemId = itemId,
            bppTicketId = Id <$> bppTicketId,
            paymentUrl = paymentUrl,
            fulfillmentId = fulfillmentId,
            fromLocation = fl,
            ..
          }
