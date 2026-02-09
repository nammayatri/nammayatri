{-
statuses Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wwarn=incomplete-record-updates #-}

module Domain.Action.Beckn.FRFS.OnStatus where

import qualified Beckn.ACL.FRFS.Utils as Utils
import qualified BecknV2.FRFS.Enums as Spec
import Control.Applicative (liftA2, (<|>))
import qualified Data.HashMap.Strict as HashMap
import Domain.Action.Beckn.FRFS.Common
import qualified Domain.Action.Beckn.FRFS.GWLink as GWSA
import qualified Domain.Types.Extra.MerchantServiceConfig as DEMSC
import qualified Domain.Types.FRFSTicket as Ticket
import qualified Domain.Types.FRFSTicketBooking as Booking
import qualified Domain.Types.FRFSTicketBookingStatus as Booking
import qualified Domain.Types.FRFSTicketStatus as Ticket
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import Domain.Types.Merchant as Merchant
import qualified Domain.Types.PartnerOrgConfig as DPOC
import Kernel.Beam.Functions
import Kernel.External.Types (SchedulerFlow, ServiceFlow)
import Kernel.Prelude hiding (second)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified Storage.CachedQueries.Merchant as QMerch
import qualified Storage.CachedQueries.PartnerOrgConfig as CQPOC
import qualified Storage.Queries.FRFSSearch as QSearch
import qualified Storage.Queries.FRFSTicket as QTicket
import qualified Storage.Queries.FRFSTicketBooking as QTBooking
import Tools.Error
import qualified Utils.Common.JWT.Config as GW
import qualified Utils.Common.JWT.TransitClaim as TC

data DOnStatus = Booking DOrder | TicketVerification DTicketPayload

data DOnStatusResp = Async | TicketVerificationSync Ticket.FRFSTicket

validateRequest ::
  ( EncFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EsqDBReplicaFlow m r,
    ServiceFlow m r,
    SchedulerFlow r
  ) =>
  DOnStatus ->
  m (Merchant, Booking.FRFSTicketBooking)
validateRequest (Booking DOrder {..}) = do
  booking <- runInReplica $ QTBooking.findByBppOrderId (Just bppOrderId) >>= fromMaybeM (BookingDoesNotExist messageId)
  _ <- fromMaybeM (SearchRequestDoesNotExist transactionId) =<< liftA2 (<|>) (runInReplica $ QSearch.findById (Id transactionId)) (runInReplica $ QSearch.findById booking.searchId)
  let merchantId = booking.merchantId
  merchant <- QMerch.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  return (merchant, booking)
validateRequest (TicketVerification DTicketPayload {..}) = do
  ticket <- runInReplica $ QTicket.findOneByTicketNumber ticketNumber >>= fromMaybeM (TicketDoesNotExist ticketNumber)
  booking <- runInReplica $ QTBooking.findById ticket.frfsTicketBookingId >>= fromMaybeM (BookingDoesNotExist ticket.frfsTicketBookingId.getId)
  let merchantId = booking.merchantId
  merchant <- QMerch.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  return (merchant, booking)

onStatus ::
  ( EncFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EsqDBReplicaFlow m r,
    ServiceFlow m r,
    SchedulerFlow r,
    HasFlowEnv m r '["googleSAPrivateKey" ::: String]
  ) =>
  Merchant ->
  Booking.FRFSTicketBooking ->
  DOnStatus ->
  m DOnStatusResp
onStatus _merchant booking (Booking dOrder) = do
  integratedBPPConfig <- SIBC.findIntegratedBPPConfigFromEntity booking
  checkInprogress <- fetchCheckInprogress integratedBPPConfig.providerConfig
  tickets <-
    if null dOrder.tickets
      then do
        tickets <- QTicket.findAllByTicketBookingId booking.id
        pure $ map mapTicketToDTicket tickets
      else return dOrder.tickets
  statuses <- traverse (Utils.getTicketStatus booking (checkInprogress && dOrder.orderStatus == Just Spec.COMPLETE)) tickets
  statuses' <-
    case dOrder.orderStatus of
      Just Spec.COMPLETE
        | booking.status == Booking.CANCEL_INITIATED -> do
          QTBooking.updateStatusById Booking.TECHNICAL_CANCEL_REJECTED booking.id
          pure statuses
        | otherwise -> pure $ updateTicketStatuses statuses
      Just Spec.CANCELLED | not booking.customerCancelled -> do
        QTBooking.updateStatusById Booking.COUNTER_CANCELLED booking.id
        pure statuses
      _ -> pure statuses
  let googleWalletStates = map (\ticketStatus -> (ticketStatus.ticketNumber, GWSA.mapToGoogleTicketStatus ticketStatus.status)) statuses'
  traverse_ updateTicket statuses'
  whenJust booking.partnerOrgId $ \pOrgId -> do
    walletPOCfg <- do
      pOrgCfg <- CQPOC.findByIdAndCfgType pOrgId DPOC.WALLET_CLASS_NAME >>= fromMaybeM (PartnerOrgConfigNotFound pOrgId.getId $ show DPOC.WALLET_CLASS_NAME)
      DPOC.getWalletClassNameConfig pOrgCfg.config
    let mbClassName = HashMap.lookup booking.merchantOperatingCityId.getId walletPOCfg.className
    whenJust mbClassName $ \_ -> fork ("updating status of tickets in google wallet for bookingId " <> booking.id.getId) $ traverse_ updateStatesForGoogleWallet googleWalletStates
  traverse_ refreshTicket tickets
  return Async
  where
    mapTicketToDTicket :: Ticket.FRFSTicket -> DTicket
    mapTicketToDTicket ticket =
      DTicket
        { qrData = ticket.qrData,
          vehicleNumber = ticket.scannedByVehicleNumber,
          description = ticket.description,
          bppFulfillmentId = Nothing,
          ticketNumber = ticket.ticketNumber,
          validTill = ticket.validTill,
          status = (mapFRFSStatusToDTicketStatus ticket.status),
          qrRefreshAt = ticket.qrRefreshAt,
          commencingHours = ticket.commencingHours,
          isReturnTicket = ticket.isReturnTicket
        }

    mapFRFSStatusToDTicketStatus :: Ticket.FRFSTicketStatus -> Text
    mapFRFSStatusToDTicketStatus = \case
      Ticket.ACTIVE -> "UNCLAIMED"
      Ticket.INPROGRESS -> "UNCLAIMED"
      Ticket.EXPIRED -> "EXPIRED"
      Ticket.USED -> "CLAIMED"
      Ticket.CANCELLED -> "CANCELLED"
      Ticket.COUNTER_CANCELLED -> "CANCELLED"
      Ticket.CANCEL_INITIATED -> "CANCELLED"
      Ticket.TECHNICAL_CANCEL_REJECTED -> "UNCLAIMED"

    updateTicketStatuses :: [Utils.TicketStatus] -> [Utils.TicketStatus]
    updateTicketStatuses = fmap (\ts@Utils.TicketStatus {} -> ts {Utils.status = Ticket.USED})
    updateTicket ticketStatus =
      void $ QTicket.updateStatusByTBookingIdAndTicketNumber ticketStatus.status ticketStatus.vehicleNumber booking.id ticketStatus.ticketNumber
    updateStatesForGoogleWallet (ticketNumber, state') = do
      let serviceName = DEMSC.WalletService GW.GoogleWallet
      let mId = booking.merchantId
      let mocId' = booking.merchantOperatingCityId
      serviceAccount <- GWSA.getserviceAccount mId mocId' serviceName
      ticket <- runInReplica $ QTicket.findByTicketBookingIdTicketNumber booking.id ticketNumber >>= fromMaybeM (InternalError "Ticket Does Not Exist")
      let resourceId = serviceAccount.saIssuerId <> "." <> ticket.id.getId
      let obj = TC.TransitObjectPatch {TC.state = show state'}
      resp <- GWSA.getObjectGoogleWallet serviceAccount resourceId
      case resp of
        Nothing -> return ()
        Just _ -> do
          void $ GWSA.updateTicketStatusForGoogleWallet obj serviceAccount resourceId
          return ()
      return ()
    refreshTicket ticket =
      whenJust ticket.qrRefreshAt $ \qrRefreshAt ->
        void $ QTicket.updateRefreshTicketQRByTBookingIdAndTicketNumber ticket.qrData (Just qrRefreshAt) booking.id ticket.ticketNumber
    fetchCheckInprogress = \case
      DIBC.ONDC _ -> do
        let key = Utils.mkCheckInprogressKey booking.searchId.getId
        fromMaybe True <$> Redis.get key >>= \val -> when val (Redis.del key) >> pure val
      _ -> pure True
onStatus _merchant booking (TicketVerification ticketPayload) = do
  ticket <- runInReplica $ QTicket.findByTicketBookingIdTicketNumber booking.id ticketPayload.ticketNumber >>= fromMaybeM (InternalError "Ticket Does Not Exist")
  let terminalTicketStates = [Ticket.USED, Ticket.EXPIRED, Ticket.CANCELLED]
  unless (ticket.status `elem` terminalTicketStates) $
    void $ QTicket.updateStatusByTBookingIdAndTicketNumber Ticket.USED Nothing booking.id ticket.ticketNumber
  return $ TicketVerificationSync ticket
