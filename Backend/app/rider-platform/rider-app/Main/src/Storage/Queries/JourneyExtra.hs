{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.JourneyExtra where

import Data.List (minimumBy, sortBy)
import Data.Ord (comparing)
import Data.Time hiding (getCurrentTime)
import qualified Domain.Types.FRFSTicket as DTicket
import qualified Domain.Types.Journey as DJ
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Journey as Beam
import qualified Storage.Queries.FRFSTicket as QTicket
import qualified Storage.Queries.FRFSTicketBooking as QTicketBooking
import Storage.Queries.OrphanInstances.Journey

-- Extra code goes here --
findAllActiveByRiderId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Kernel.Types.Id.Id Domain.Types.Person.Person -> Maybe Bool -> m [DJ.Journey]
findAllActiveByRiderId riderId mbIsPaymentSuccess = do
  now <- getCurrentTime
  let lastDay = addUTCTime (-60 * 60 * 24) now
      isPaymentSuccess = fromMaybe True mbIsPaymentSuccess
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.status $ Se.In [Just DJ.CONFIRMED, Just DJ.INPROGRESS, Just DJ.FEEDBACK_PENDING],
          Se.Is Beam.riderId $ Se.Eq (Kernel.Types.Id.getId riderId),
          Se.Is Beam.createdAt $ Se.GreaterThanOrEq lastDay,
          Se.Or
            [ Se.Is Beam.isPublicTransportIncluded $ Se.Eq (Just False),
              Se.And
                [ Se.Is Beam.isPublicTransportIncluded $ Se.Eq (Just True),
                  Se.Is Beam.isPaymentSuccess $ Se.Eq (Just isPaymentSuccess)
                ]
            ]
        ]
    ]

findAllByRiderId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.Person.Person -> Maybe Integer -> Maybe Integer -> Maybe UTCTime -> Maybe UTCTime -> [DJ.JourneyStatus] -> Maybe Bool -> m [DJ.Journey]
findAllByRiderId (Kernel.Types.Id.Id personId) mbLimit mbOffset mbFromDate mbToDate mbJourneyStatusList mbIsPaymentSuccess = do
  let limit' = maybe 10 fromIntegral mbLimit
  let offset' = maybe 0 fromIntegral mbOffset
      mbJourneyStatus = Just <$> mbJourneyStatusList
  journeys <-
    findAllWithOptionsKV
      [ Se.And
          ( [Se.Is Beam.riderId $ Se.Eq personId]
              <> ([Se.Is Beam.createdAt $ Se.GreaterThanOrEq (fromJust mbFromDate) | isJust mbFromDate])
              <> ([Se.Is Beam.createdAt $ Se.LessThanOrEq (fromJust mbToDate) | isJust mbToDate])
              <> ([Se.Is Beam.status $ Se.Not $ Se.In [Just DJ.NEW, Just DJ.INITIATED]])
              <> ([Se.Is Beam.status $ Se.In mbJourneyStatus | not (null mbJourneyStatus)])
              <> [ Se.Or
                     [ Se.Is Beam.isPublicTransportIncluded $ Se.Eq (Just False),
                       Se.And
                         [ Se.Is Beam.isPublicTransportIncluded $ Se.Eq (Just True),
                           Se.Is Beam.isPaymentSuccess $ Se.Eq mbIsPaymentSuccess
                         ]
                     ]
                 ]
          )
      ]
      (Se.Desc Beam.startTime)
      (Just limit')
      (Just offset')
  pure journeys

findAllByRiderIdAndStatusAndMOCId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.Person.Person -> DJ.JourneyStatus -> Kernel.Types.Id.Id DMOC.MerchantOperatingCity -> m [DJ.Journey]
findAllByRiderIdAndStatusAndMOCId (Kernel.Types.Id.Id personId) status (Kernel.Types.Id.Id mocId) = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.riderId $ Se.Eq personId,
          Se.Is Beam.status $ Se.Eq $ Just status,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Just mocId)
        ]
    ]
    (Se.Desc Beam.createdAt)
    (Just 10)
    Nothing

updateJourneyExpiryTime :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Kernel.Types.Id.Id DJ.Journey -> UTCTime -> m ()
updateJourneyExpiryTime journeyId expiryTime = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.journeyExpiryTime $ Just expiryTime, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId journeyId)]

minimumByMay' :: (a -> a -> Ordering) -> [a] -> Maybe a
minimumByMay' _ [] = Nothing
minimumByMay' cmp xs = Just (minimumBy cmp xs)

-- Update journey expiry time with provided tickets (avoids additional DB queries)
updateShortestJourneyExpiryTimeWithTickets ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id DJ.Journey ->
  [DTicket.FRFSTicket] ->
  m ()
updateShortestJourneyExpiryTimeWithTickets journeyId tickets =
  forM_ (minimumByMay' (comparing (.validTill)) tickets) $ \ticket -> do
    updateJourneyExpiryTime journeyId ticket.validTill

-- Update journey expiry time to the next valid ticket expiry when a leg is completed
updateJourneyToNextTicketExpiryTime ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id DJ.Journey ->
  Int ->
  m ()
updateJourneyToNextTicketExpiryTime journeyId nextLegSequence = do
  -- Get all bookings for the journey and filter upcoming ones
  bookings <- QTicketBooking.findAllByJourneyId (Just journeyId)
  let upcomingBookingIds = mapMaybe (\b -> b.journeyLegOrder >>= \o -> if o >= nextLegSequence then Just b.id else Nothing) bookings
  -- Fetch all tickets from those bookings
  tickets <- fmap concat $ mapM QTicket.findAllByTicketBookingId upcomingBookingIds
  -- Find the earliest ticket expiry
  updateShortestJourneyExpiryTimeWithTickets journeyId tickets
