module Storage.Queries.RideBooking where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common
import Beckn.Types.Id
import Domain.Types.Person (Person)
import Domain.Types.Quote (Quote)
import Domain.Types.RideBooking as DRB
import Domain.Types.SearchRequest (SearchRequest)
import Storage.Tabular.RideBooking
import Storage.Tabular.SearchRequest ()

create :: RideBooking -> SqlDB ()
create = create'

updateStatus :: Id RideBooking -> RideBookingStatus -> SqlDB ()
updateStatus rbId rbStatus = do
  now <- getCurrentTime
  update' $ \tbl -> do
    set
      tbl
      [ RideBookingUpdatedAt =. val now,
        RideBookingStatus =. val rbStatus
      ]
    where_ $ tbl ^. RideBookingId ==. val (getId rbId)

updateBPPBookingId :: Id RideBooking -> Id BPPRideBooking -> SqlDB ()
updateBPPBookingId rbId bppRbId = do
  now <- getCurrentTime
  update' $ \tbl -> do
    set
      tbl
      [ RideBookingUpdatedAt =. val now,
        RideBookingBppBookingId =. val (Just $ getId bppRbId)
      ]
    where_ $ tbl ^. RideBookingId ==. val (getId rbId)

findById :: EsqDBFlow m r => Id RideBooking -> m (Maybe RideBooking)
findById = Esq.findById

findByBPPBookingId :: EsqDBFlow m r => Id BPPRideBooking -> m (Maybe RideBooking)
findByBPPBookingId bppRbId =
  runTransaction . findOne' $ do
    rideBooking <- from $ table @RideBookingT
    where_ $ rideBooking ^. RideBookingBppBookingId ==. val (Just $ getId bppRbId)
    return rideBooking

findByQuoteId :: EsqDBFlow m r => Id Quote -> m (Maybe RideBooking)
findByQuoteId quoteId_ =
  runTransaction . findOne' $ do
    rideBooking <- from $ table @RideBookingT
    where_ $ rideBooking ^. RideBookingQuoteId ==. val (toKey quoteId_)
    return rideBooking

findByRequestId :: EsqDBFlow m r => Id SearchRequest -> m (Maybe RideBooking)
findByRequestId searchRequestId =
  runTransaction . findOne' $ do
    rideBooking <- from $ table @RideBookingT
    where_ $ rideBooking ^. RideBookingRequestId ==. val (toKey searchRequestId)
    return rideBooking

findAllByRiderId :: EsqDBFlow m r => Id Person -> Maybe Integer -> Maybe Integer -> Maybe Bool -> m [RideBooking]
findAllByRiderId personId mbLimit mbOffset mbOnlyActive = do
  let isOnlyActive = Just True == mbOnlyActive
  runTransaction . findAll' $ do
    rideBooking <- from $ table @RideBookingT
    where_ $
      rideBooking ^. RideBookingRiderId ==. val (toKey personId)
        &&. whenTrue_ isOnlyActive (not_ (rideBooking ^. RideBookingStatus `in_` valList [DRB.COMPLETED, DRB.CANCELLED]))
    limit $ fromIntegral $ fromMaybe 10 mbLimit
    offset $ fromIntegral $ fromMaybe 0 mbOffset
    orderBy [desc $ rideBooking ^. RideBookingCreatedAt]
    return rideBooking
