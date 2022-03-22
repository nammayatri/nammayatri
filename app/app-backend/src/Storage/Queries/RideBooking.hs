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

findById :: Transactionable m => Id RideBooking -> m (Maybe RideBooking)
findById = Esq.findById

findByBPPBookingId :: Transactionable m => Id BPPRideBooking -> m (Maybe RideBooking)
findByBPPBookingId bppRbId =
  findOne $ do
    rideBooking <- from $ table @RideBookingT
    where_ $ rideBooking ^. RideBookingBppBookingId ==. val (Just $ getId bppRbId)
    return rideBooking

findByQuoteId :: Transactionable m => Id Quote -> m (Maybe RideBooking)
findByQuoteId quoteId_ =
  findOne $ do
    rideBooking <- from $ table @RideBookingT
    where_ $ rideBooking ^. RideBookingQuoteId ==. val (toKey quoteId_)
    return rideBooking

findByRequestId :: Transactionable m => Id SearchRequest -> m (Maybe RideBooking)
findByRequestId searchRequestId =
  findOne $ do
    rideBooking <- from $ table @RideBookingT
    where_ $ rideBooking ^. RideBookingRequestId ==. val (toKey searchRequestId)
    return rideBooking

findAllByRiderId :: Transactionable m => Id Person -> Maybe Integer -> Maybe Integer -> Maybe Bool -> m [RideBooking]
findAllByRiderId personId mbLimit mbOffset mbOnlyActive = do
  let isOnlyActive = Just True == mbOnlyActive
  findAll $ do
    rideBooking <- from $ table @RideBookingT
    where_ $
      rideBooking ^. RideBookingRiderId ==. val (toKey personId)
        &&. whenTrue_ isOnlyActive (not_ (rideBooking ^. RideBookingStatus `in_` valList [DRB.COMPLETED, DRB.CANCELLED]))
    limit $ fromIntegral $ fromMaybe 10 mbLimit
    offset $ fromIntegral $ fromMaybe 0 mbOffset
    orderBy [desc $ rideBooking ^. RideBookingCreatedAt]
    return rideBooking
