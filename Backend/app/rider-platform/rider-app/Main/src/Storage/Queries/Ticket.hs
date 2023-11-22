{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Ticket where

import qualified Domain.Types.LocationMapping as DLM
import Domain.Types.Ticket
import EulerHS.Prelude (whenNothingM_)
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified SharedLogic.LocationMapping as SLM
import qualified Storage.Beam.Ticket as BeamT
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.LocationMapping as QLM

createTicket' :: MonadFlow m => Ticket -> m ()
createTicket' = createWithKV

create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Ticket -> m ()
create dTicket = do
  _ <- whenNothingM_ (QL.findById dTicket.fromLocation.id) $ do QL.create (dTicket.fromLocation)
  void $ createTicket' dTicket

createTicket :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Ticket -> m ()
createTicket ticket = do
  fromLocationMap <- SLM.buildPickUpLocationMapping ticket.fromLocation.id ticket.id.getId DLM.BOOKING Nothing (Just ticket.merchantOperatingCityId) -- Create a DLM.TICKET type?
  void $ QLM.create fromLocationMap
  create ticket

findAll :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => m [Ticket]
findAll = findAllWithKV [Se.Is BeamT.id $ Se.Not $ Se.Eq ""]

findById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Ticket -> m (Maybe Ticket)
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

findByBPPTicketId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id BPPTicket -> m (Maybe Ticket)
findByBPPTicketId (Id bppRbId) = findOneWithKV [Se.Is BeamT.bppTicketId $ Se.Eq $ Just bppRbId]

findByTransactionId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> m (Maybe Ticket)
findByTransactionId transactionId = findOneWithKV [Se.Is BeamT.searchRequestId $ Se.Eq transactionId]

instance ToTType' BeamT.Ticket Ticket where
  toTType' Ticket {..} = do
    BeamT.TicketT
      { BeamT.id = getId id,
        BeamT.status = status,
        BeamT.quoteId = getId <$> quoteId,
        BeamT.itemId = itemId,
        BeamT.fulfillmentId = fulfillmentId,
        BeamT.paymentUrl = paymentUrl,
        BeamT.searchRequestId = searchRequestId,
        BeamT.bppOrderId = bppOrderId,
        BeamT.quantity = quantity,
        BeamT.fromLocationId = getId fromLocation.id,
        BeamT.pricePerAdult = pricePerAdult,
        BeamT.totalPrice = totalPrice,
        BeamT.qrData = qrData,
        BeamT.merchantOperatingCityId = getId merchantOperatingCityId,
        BeamT.providerId = providerId,
        BeamT.providerUrl = showBaseUrl providerUrl,
        BeamT.bppTicketId = getId <$> bppTicketId,
        BeamT.createdAt = createdAt,
        BeamT.updatedAt = updatedAt
      }

instance FromTType' BeamT.Ticket Ticket where
  fromTType' BeamT.TicketT {..} = do
    pUrl <- parseBaseUrl providerUrl
    mappings <- QLM.findByEntityId id
    let fromLocationMapping = filter (\loc -> loc.order == 0) mappings
    fromLocMap <- listToMaybe fromLocationMapping & fromMaybeM (InternalError "Entity Mappings For FromLocation Not Found")
    fl <- QL.findById fromLocMap.locationId >>= fromMaybeM (InternalError $ "fromLocation not found in ticket for fromLocationId: " <> show fromLocationId)
    pure $
      Just
        Ticket
          { id = Id id,
            quoteId = Id <$> quoteId,
            searchRequestId = searchRequestId,
            bppOrderId = bppOrderId,
            pricePerAdult = pricePerAdult,
            totalPrice = totalPrice,
            qrData = qrData,
            merchantOperatingCityId = Id merchantOperatingCityId,
            providerId = providerId,
            providerUrl = pUrl,
            itemId = itemId,
            bppTicketId = Id <$> bppTicketId,
            paymentUrl = paymentUrl,
            fulfillmentId = fulfillmentId,
            fromLocation = fl,
            ..
          }
