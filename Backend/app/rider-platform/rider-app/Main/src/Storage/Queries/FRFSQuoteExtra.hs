module Storage.Queries.FRFSQuoteExtra where

import Domain.Types.FRFSQuote
import Domain.Types.FRFSSearch
import Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSQuote as Beam
import Storage.Queries.OrphanInstances.FRFSQuote ()

-- Extra code goes here --

backfillQuotesForCachedQuoteFlow :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> Int -> Maybe Int -> Maybe HighPrecMoney -> Maybe Bool -> Kernel.Types.Id.Id Domain.Types.FRFSSearch.FRFSSearch -> m ())
backfillQuotesForCachedQuoteFlow riderId quantity discountedTickets eventDiscountAmount isEventOngoing searchId = do
  _now <- getCurrentTime
  updateWithKV
    ( [Se.Set Beam.riderId (Kernel.Types.Id.getId riderId), Se.Set Beam.updatedAt _now, Se.Set Beam.quantity (Just quantity)]
        <> ([Se.Set Beam.discountedTickets discountedTickets | (Just True) == isEventOngoing])
        <> ([Se.Set Beam.eventDiscountAmount eventDiscountAmount | (Just True) == isEventOngoing])
    )
    [Se.Is Beam.searchId $ Se.Eq (Kernel.Types.Id.getId searchId)]

updateCachedQuoteByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (FRFSQuote -> m ())
updateCachedQuoteByPrimaryKey FRFSQuote {..} = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.bppDelayedInterest bppDelayedInterest,
      Se.Set Beam.bppItemId bppItemId,
      Se.Set Beam.bppSubscriberId bppSubscriberId,
      Se.Set Beam.bppSubscriberUrl bppSubscriberUrl,
      Se.Set Beam.discountedTickets discountedTickets,
      Se.Set Beam.eventDiscountAmount eventDiscountAmount,
      Se.Set Beam.oldCacheDump oldCacheDump,
      Se.Set Beam.currency ((.currency) <$> price),
      Se.Set Beam.price ((.amount) <$> price),
      Se.Set Beam.providerDescription providerDescription,
      Se.Set Beam.providerId providerId,
      Se.Set Beam.providerName providerName,
      Se.Set Beam.quantity quantity,
      Se.Set Beam.routeStationsJson routeStationsJson,
      Se.Set Beam.stationsJson stationsJson,
      Se.Set Beam.validTill validTill,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updatePriceAndEstimatedPriceById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote -> Price -> Maybe Price -> m ())
updatePriceAndEstimatedPriceById id price quotePriceBeforeUpdate = do
  _now <- getCurrentTime
  updateWithKV
    ([Se.Set Beam.price (Just ((.amount) price)), Se.Set Beam.estimatedPrice (Kernel.Prelude.fmap (.amount) quotePriceBeforeUpdate), Se.Set Beam.updatedAt _now])
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateValidTillById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote -> UTCTime -> m ())
updateValidTillById id validTill = do
  _now <- getCurrentTime
  updateWithKV
    [Se.Set Beam.validTill validTill, Se.Set Beam.updatedAt _now]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateTicketAndChildTicketQuantityById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote -> Maybe Int -> Maybe Int -> m ())
updateTicketAndChildTicketQuantityById id quantity childTicketQuantity = do
  _now <- getCurrentTime
  updateWithKV
    ([Se.Set Beam.updatedAt _now] <> [Se.Set Beam.quantity quantity | isJust quantity] <> [Se.Set Beam.childTicketQuantity childTicketQuantity | isJust childTicketQuantity])
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]
