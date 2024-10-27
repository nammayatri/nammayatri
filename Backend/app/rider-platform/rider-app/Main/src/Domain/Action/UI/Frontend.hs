{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Frontend
  ( GetPersonFlowStatusRes,
    FrontendEvent (..),
    NotifyEventReq (..),
    NotifyEventResp,
    getPersonFlowStatus,
    notifyEvent,
  )
where

import qualified Data.HashMap.Strict as HM
import Domain.Action.UI.Booking
import Domain.Action.UI.Quote
import Domain.Types.CancellationReason
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.PersonFlowStatus as DPFS
import Environment
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Types.APISuccess (APISuccess)
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QPFS
import qualified Storage.CachedQueries.ValueAddNP as QNP
import qualified Storage.Queries.Booking as QB
import TransactionLogs.Types

data GetPersonFlowStatusRes = GetPersonFlowStatusRes
  { oldStatus :: Maybe DPFS.FlowStatus,
    currentStatus :: DPFS.FlowStatus,
    isValueAddNP :: Maybe Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data FrontendEvent = RATE_DRIVER_SKIPPED | SEARCH_CANCELLED
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype NotifyEventReq = NotifyEventReq
  { event :: FrontendEvent
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

type NotifyEventResp = APISuccess

getPersonFlowStatus :: Id DP.Person -> Id DM.Merchant -> Maybe Bool -> Maybe Bool -> Flow GetPersonFlowStatusRes
getPersonFlowStatus personId merchantId _ pollActiveBooking = do
  personStatus' <- QPFS.getStatus personId
  case personStatus' of
    Just personStatus -> do
      case personStatus of
        DPFS.WAITING_FOR_DRIVER_OFFERS _ _ _ providerId -> findValueAddNP personStatus providerId
        DPFS.WAITING_FOR_DRIVER_ASSIGNMENT _ _ _ _ -> expirePersonStatusIfNeeded personStatus Nothing
        _ -> checkForActiveBooking
    Nothing -> checkForActiveBooking
  where
    checkForActiveBooking :: Flow GetPersonFlowStatusRes
    checkForActiveBooking = do
      if (isJust pollActiveBooking)
        then do
          activeBookings <- bookingList (personId, merchantId) Nothing Nothing (Just True) Nothing Nothing
          if null activeBookings.list
            then return $ GetPersonFlowStatusRes Nothing (DPFS.ACTIVE_BOOKINGS activeBookings.list) Nothing
            else do
              pendingFeedbackBookings <- bookingList (personId, merchantId) (Just 1) Nothing (Just False) Nothing Nothing
              case pendingFeedbackBookings.list of
                [booking] -> do
                  let isRated = isJust $ booking.rideList & listToMaybe >>= (.rideRating)
                  if isRated
                    then return $ GetPersonFlowStatusRes Nothing DPFS.IDLE Nothing
                    else return $ GetPersonFlowStatusRes Nothing (DPFS.FEEDBACK_PENDING booking) Nothing
                _ -> return $ GetPersonFlowStatusRes Nothing DPFS.IDLE Nothing
        else return $ GetPersonFlowStatusRes Nothing DPFS.IDLE Nothing

    findValueAddNP personStatus providerId = do
      isValueAddNP_ <- maybe (pure True) QNP.isValueAddNP providerId
      expirePersonStatusIfNeeded personStatus (Just isValueAddNP_)

    expirePersonStatusIfNeeded personStatus isValueAddNp = do
      now <- getCurrentTime
      if now < personStatus.validTill
        then return $ GetPersonFlowStatusRes Nothing personStatus isValueAddNp
        else do
          _ <- QPFS.updateStatus personId DPFS.IDLE
          return $ GetPersonFlowStatusRes (Just personStatus) DPFS.IDLE isValueAddNp

notifyEvent :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r, HasField "shortDurationRetryCfg" r RetryCfg, HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl], HasFlowEnv m r '["nwAddress" ::: BaseUrl], Esq.EsqDBReplicaFlow m r, MonadFlow m, HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools], HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig]) => Id DP.Person -> NotifyEventReq -> m NotifyEventResp
notifyEvent personId req = do
  _ <- case req.event of
    RATE_DRIVER_SKIPPED -> QPFS.updateStatus personId DPFS.IDLE
    SEARCH_CANCELLED -> do
      activeBooking <- B.runInReplica $ QB.findLatestSelfAndPartyBookingByRiderId personId
      whenJust activeBooking $ \booking -> processActiveBooking booking OnSearch
      QPFS.updateStatus personId DPFS.IDLE
  pure APISuccess.Success
