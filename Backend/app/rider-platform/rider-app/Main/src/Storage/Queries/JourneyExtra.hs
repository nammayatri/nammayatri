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
              <> foldMap (\d -> [Se.Is Beam.createdAt $ Se.GreaterThanOrEq d]) mbFromDate
              <> foldMap (\d -> [Se.Is Beam.createdAt $ Se.LessThanOrEq d]) mbToDate
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

-- Update journey expiry time with provided tickets (avoids additional DB queries)
updateLongestJourneyExpiryTimeWithTickets ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id DJ.Journey ->
  [DTicket.FRFSTicket] ->
  m ()
updateLongestJourneyExpiryTimeWithTickets journeyId tickets =
  forM_ (maximumByMay' (comparing (.validTill)) tickets) $ \ticket -> do
    updateJourneyExpiryTime journeyId ticket.validTill

-- Update journey status and end time when journey is completed
updateStatusAndEndTime :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => DJ.JourneyStatus -> Kernel.Types.Id.Id DJ.Journey -> m ()
updateStatusAndEndTime status journeyId = do
  _now <- getCurrentTime
  let endTimeUpdate = ([Se.Set Beam.endTime $ Just _now | status == DJ.FEEDBACK_PENDING])
  updateOneWithKV
    ([Se.Set Beam.status $ Just status, Se.Set Beam.updatedAt _now] <> endTimeUpdate)
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId journeyId)]
