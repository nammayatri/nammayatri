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

-- Extra code goes here --

backfillQuotesForCachedQuoteFlow :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> Int -> Maybe Int -> Maybe HighPrecMoney -> Maybe Bool -> Kernel.Types.Id.Id Domain.Types.FRFSSearch.FRFSSearch -> m ())
backfillQuotesForCachedQuoteFlow riderId quantity discountedTickets eventDiscountAmount isEventOngoing searchId = do
  _now <- getCurrentTime
  updateWithKV
    ( [Se.Set Beam.riderId (Kernel.Types.Id.getId riderId), Se.Set Beam.updatedAt _now, Se.Set Beam.quantity quantity]
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
      Se.Set Beam.discountsJson discountsJson,
      Se.Set Beam.eventDiscountAmount eventDiscountAmount,
      Se.Set Beam.oldCacheDump oldCacheDump,
      Se.Set Beam.currency ((Kernel.Prelude.Just . (.currency)) price),
      Se.Set Beam.price ((.amount) price),
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
