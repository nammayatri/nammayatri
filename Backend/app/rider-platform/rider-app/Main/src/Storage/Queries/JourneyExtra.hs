{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.JourneyExtra where

import Control.Monad.Extra (mapMaybeM)
import Data.List (maximumBy, sortBy)
import Data.Ord (comparing)
import Data.Time hiding (getCurrentTime)
import qualified Domain.Types.FRFSTicket as DTicket
import qualified Domain.Types.Journey as DJ
import qualified Domain.Types.JourneyLeg as JL
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Common (Seconds)
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime, logDebug)
import qualified Sequelize as Se
import qualified Storage.Beam.Journey as Beam
import qualified Storage.Queries.FRFSTicket as QTicket
import qualified Storage.Queries.FRFSTicketBooking as QTicketBooking
import Storage.Queries.OrphanInstances.Journey

-- Extra code goes here --
findAllActiveByRiderId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Kernel.Types.Id.Id Domain.Types.Person.Person -> m [DJ.Journey]
findAllActiveByRiderId riderId = do
  now <- getCurrentTime
  let lastDay = addUTCTime (-60 * 60 * 24) now
  findAllWithKV [Se.And [Se.Is Beam.status $ Se.In [Just DJ.CONFIRMED, Just DJ.INPROGRESS, Just DJ.FEEDBACK_PENDING], Se.Is Beam.riderId $ Se.Eq (Kernel.Types.Id.getId riderId), Se.Is Beam.createdAt $ Se.GreaterThanOrEq lastDay]]

findAllOlderActiveByRiderId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Kernel.Types.Id.Id Domain.Types.Person.Person -> Int -> m [DJ.Journey]
findAllOlderActiveByRiderId riderId limit = do
  now <- getCurrentTime
  let lastDay = addUTCTime (-60 * 60 * 24) now
  findAllWithOptionsKV
    [Se.And [Se.Is Beam.status $ Se.In [Just DJ.CONFIRMED, Just DJ.INPROGRESS, Just DJ.FEEDBACK_PENDING], Se.Is Beam.riderId $ Se.Eq (Kernel.Types.Id.getId riderId), Se.Is Beam.createdAt $ Se.LessThan lastDay]]
    (Se.Desc Beam.createdAt)
    (Just limit)
    Nothing

findTodaysPublicTransportByRiderId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Kernel.Types.Id.Id Domain.Types.Person.Person -> UTCTime -> UTCTime -> UTCTime -> m [DJ.Journey]
findTodaysPublicTransportByRiderId riderId liveSince dayStart dayEnd =
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.riderId $ Se.Eq (Kernel.Types.Id.getId riderId),
          Se.Is Beam.isPublicTransportIncluded $ Se.Eq (Just True),
          Se.Or
            [ Se.And
                [ Se.Is Beam.status $ Se.Eq (Just DJ.INPROGRESS),
                  Se.Is Beam.startTime $ Se.GreaterThanOrEq (Just liveSince)
                ],
              Se.And
                [ Se.Is Beam.status $ Se.Eq (Just DJ.CONFIRMED),
                  Se.Is Beam.isPaymentSuccess $ Se.Eq (Just True),
                  Se.Is Beam.startTime $ Se.GreaterThanOrEq (Just dayStart),
                  Se.Is Beam.startTime $ Se.LessThan (Just dayEnd)
                ]
            ]
        ]
    ]
    (Se.Asc Beam.startTime)
    Nothing
    Nothing

findLatestFeedbackPendingByRiderId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Kernel.Types.Id.Id Domain.Types.Person.Person -> UTCTime -> m (Maybe DJ.Journey)
findLatestFeedbackPendingByRiderId riderId since =
  listToMaybe
    <$> findAllWithOptionsKV
      [ Se.And
          [ Se.Is Beam.riderId $ Se.Eq (Kernel.Types.Id.getId riderId),
            Se.Is Beam.isPublicTransportIncluded $ Se.Eq (Just True),
            Se.Is Beam.status $ Se.Eq (Just DJ.FEEDBACK_PENDING),
            Se.Is Beam.endTime $ Se.GreaterThanOrEq (Just since)
          ]
      ]
      (Se.Desc Beam.endTime)
      (Just 1)
      Nothing

findStaleActiveByRiderId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Kernel.Types.Id.Id Domain.Types.Person.Person -> UTCTime -> UTCTime -> UTCTime -> Int -> m [DJ.Journey]
findStaleActiveByRiderId riderId now unpaidBefore feedbackBefore limit =
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.riderId $ Se.Eq (Kernel.Types.Id.getId riderId),
          Se.Or
            [ Se.And
                [ Se.Is Beam.status $ Se.Eq (Just DJ.CONFIRMED),
                  Se.Is Beam.isPublicTransportIncluded $ Se.Eq (Just True),
                  Se.Or [Se.Is Beam.isPaymentSuccess $ Se.Eq Nothing, Se.Is Beam.isPaymentSuccess $ Se.Eq (Just False)],
                  Se.Is Beam.createdAt $ Se.LessThan unpaidBefore
                ],
              Se.And
                [ Se.Is Beam.status $ Se.Eq (Just DJ.FEEDBACK_PENDING),
                  Se.Is Beam.endTime $ Se.Not $ Se.Eq Nothing,
                  Se.Is Beam.endTime $ Se.LessThan (Just feedbackBefore)
                ],
              Se.And
                [ Se.Is Beam.status $ Se.In [Just DJ.CONFIRMED, Just DJ.INPROGRESS],
                  Se.Is Beam.journeyExpiryTime $ Se.Not $ Se.Eq Nothing,
                  Se.Is Beam.journeyExpiryTime $ Se.LessThan (Just now)
                ]
            ]
        ]
    ]
    (Se.Desc Beam.createdAt)
    (Just limit)
    Nothing

findAllByRiderId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.Person.Person -> Maybe Integer -> Maybe Integer -> Maybe UTCTime -> Maybe UTCTime -> [DJ.JourneyStatus] -> Maybe Bool -> m [DJ.Journey]
findAllByRiderId (Kernel.Types.Id.Id personId) mbLimit mbOffset mbFromDate mbToDate mbJourneyStatusList mbIsPaymentSuccess = do
  logDebug $ "myrides getJourneyList PersonId: " <> show personId <> " mbLimit: " <> show mbLimit <> " mbOffset: " <> show mbOffset <> " mbFromDate: " <> show mbFromDate <> " mbToDate: " <> show mbToDate <> " mbJourneyStatusList: " <> show mbJourneyStatusList <> " mbIsPaymentSuccess: " <> show mbIsPaymentSuccess
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
              <> [ Se.And
                     [ Se.Is Beam.isPublicTransportIncluded $ Se.Not $ Se.Eq (Just False),
                       Se.Is Beam.isPaymentSuccess $ Se.Eq mbIsPaymentSuccess
                     ]
                 ]
          )
      ]
      (Se.Desc Beam.createdAt)
      (Just limit')
      (Just offset')
  pure journeys

findAllByRiderIdAndStatusAndMOCId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.Person.Person -> DJ.JourneyStatus -> Kernel.Types.Id.Id DMOC.MerchantOperatingCity -> m [DJ.Journey]
findAllByRiderIdAndStatusAndMOCId (Kernel.Types.Id.Id personId) status (Kernel.Types.Id.Id mocId) = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.riderId $ Se.Eq personId,
          Se.Is Beam.status $ Se.Eq $ Just status,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq mocId
        ]
    ]
    (Se.Desc Beam.createdAt)
    (Just 10)
    Nothing

updateJourneyExpiryTime :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Kernel.Types.Id.Id DJ.Journey -> UTCTime -> m ()
updateJourneyExpiryTime journeyId expiryTime = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.journeyExpiryTime $ Just expiryTime, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId journeyId)]

maximumByMay' :: (a -> a -> Ordering) -> [a] -> Maybe a
maximumByMay' _ [] = Nothing
maximumByMay' cmp xs = Just (maximumBy cmp xs)

-- Update journey expiry time with provided tickets, never moving it earlier: a journey with several
-- FRFS legs gets one call per leg, so keep the latest validity seen across all of them.
updateLongestJourneyExpiryTimeWithTickets ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id DJ.Journey ->
  [DTicket.FRFSTicket] ->
  m ()
updateLongestJourneyExpiryTimeWithTickets journeyId tickets =
  forM_ (maximumByMay' (comparing (.validTill)) tickets) $ \ticket -> do
    mbJourney :: Maybe DJ.Journey <- findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId journeyId)]
    let expiryTime = maybe ticket.validTill (max ticket.validTill) (mbJourney >>= (.journeyExpiryTime))
    updateJourneyExpiryTime journeyId expiryTime

-- Update journey status and end time when journey is completed
updateStatusAndEndTime :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => DJ.JourneyStatus -> Kernel.Types.Id.Id DJ.Journey -> m ()
updateStatusAndEndTime status journeyId = do
  _now <- getCurrentTime
  let endTimeUpdate = ([Se.Set Beam.endTime $ Just _now | status == DJ.FEEDBACK_PENDING])
  updateOneWithKV
    ([Se.Set Beam.status $ Just status, Se.Set Beam.updatedAt _now] <> endTimeUpdate)
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId journeyId)]

updateTripTimesAndStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => DJ.JourneyStatus -> Maybe UTCTime -> Maybe UTCTime -> Maybe Seconds -> Kernel.Types.Id.Id DJ.Journey -> m ()
updateTripTimesAndStatus status startTime endTime estimatedDuration journeyId = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.status $ Just status,
      Se.Set Beam.startTime startTime,
      Se.Set Beam.endTime endTime,
      Se.Set Beam.estimatedDuration estimatedDuration,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId journeyId)]
