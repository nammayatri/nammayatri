{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.FRFSUtils where

import qualified API.Types.UI.FRFSTicketService as APITypes
import qualified Domain.Action.Beckn.FRFS.OnConfirm as DACFOC
import qualified Domain.Types.BecknConfig as DBC
import qualified Domain.Types.FRFSConfig as Config
import qualified Domain.Types.FRFSTicket as DT
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import qualified Domain.Types.FRFSTicketBookingPayment as DTBP
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.PartnerOrganization as DPO
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Station as Station
import qualified Environment as Environment
import EulerHS.Prelude hiding (id)
import qualified ExternalBPP.CallAPI as CallExternalBPP
import Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Payment.Interface.Types as Payment
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QPaymentOrder
import qualified Lib.Payment.Storage.Queries.PaymentTransaction as QPaymentTransaction
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.PartnerOrgStation as CQPOS
import qualified Storage.CachedQueries.Station as CQS
import qualified Storage.Queries.FRFSTicketBokingPayment as QFRFSTicketBookingPayment
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import Tools.Error

mkTicketAPI :: DT.FRFSTicket -> APITypes.FRFSTicketAPI
mkTicketAPI DT.FRFSTicket {..} = APITypes.FRFSTicketAPI {..}

mkPOrgStationAPIRes :: (CacheFlow m r, EsqDBFlow m r) => Station.Station -> Maybe (Id DPO.PartnerOrganization) -> m APITypes.FRFSStationAPI
mkPOrgStationAPIRes Station.Station {..} mbPOrgId = do
  pOrgStation <- mbPOrgId `forM` \pOrgId -> B.runInReplica $ CQPOS.findByStationIdAndPOrgId id pOrgId >>= fromMaybeM (PartnerOrgStationNotFoundForStationId pOrgId.getId id.getId)
  let pOrgStationName = pOrgStation <&> (.name)
  pure $ APITypes.FRFSStationAPI {name = fromMaybe name pOrgStationName, stationType = Nothing, color = Nothing, sequenceNum = Nothing, distance = Nothing, towards = Nothing, ..}

mkTBPStatusAPI :: DTBP.FRFSTicketBookingPaymentStatus -> APITypes.FRFSBookingPaymentStatusAPI
mkTBPStatusAPI = \case
  DTBP.PENDING -> APITypes.PENDING
  DTBP.SUCCESS -> APITypes.SUCCESS
  DTBP.FAILED -> APITypes.FAILURE
  DTBP.REFUND_PENDING -> APITypes.REFUND_PENDING
  DTBP.REFUNDED -> APITypes.REFUNDED

mkFRFSConfigAPI :: Config.FRFSConfig -> APITypes.FRFSConfigAPIRes
mkFRFSConfigAPI Config.FRFSConfig {..} = do
  APITypes.FRFSConfigAPIRes {isEventOngoing = False, ticketsBookedInEvent = 0, ..}

mkPOrgStationAPI :: (CacheFlow m r, EsqDBFlow m r) => Maybe (Id DPO.PartnerOrganization) -> Id DMOC.MerchantOperatingCity -> APITypes.FRFSStationAPI -> m APITypes.FRFSStationAPI
mkPOrgStationAPI mbPOrgId merchantOperatingCityId stationAPI = do
  station <- B.runInReplica $ CQS.findByStationCodeAndMerchantOperatingCityId stationAPI.code merchantOperatingCityId >>= fromMaybeM (StationNotFound $ "station code:" +|| stationAPI.code ||+ "and merchantOperatingCityId: " +|| merchantOperatingCityId ||+ "")
  mkPOrgStationAPIRes station mbPOrgId

handleConfirmProcess ::
  DM.Merchant ->
  DMOC.MerchantOperatingCity ->
  DBC.BecknConfig ->
  DFRFSTicketBooking.FRFSTicketBooking ->
  DP.Person ->
  DPayment.PaymentStatusResp ->
  Kernel.Prelude.UTCTime ->
  Environment.Flow DFRFSTicketBooking.FRFSTicketBooking
handleConfirmProcess merchant merchantOperatingCity bapConfig booking person paymentStatusResp now = do
  paymentBooking <- B.runInReplica $ QFRFSTicketBookingPayment.findNewTBPByBookingId booking.id >>= fromMaybeM (InvalidRequest "Payment booking not found for approved TicketBookingId")
  paymentOrder <- QPaymentOrder.findById paymentBooking.paymentOrderId >>= fromMaybeM (InvalidRequest "Payment order not found for approved TicketBookingId")
  let updatedTTL = addUTCTime (maybe 60 intToNominalDiffTime bapConfig.confirmTTLSec) now
  transactions <- QPaymentTransaction.findAllByOrderId paymentOrder.id
  txnId <- getSuccessTransactionId transactions
  void $ QFRFSTicketBookingPayment.updateStatusByTicketBookingId DTBP.SUCCESS booking.id
  void $ QFRFSTicketBooking.updateStatusValidTillAndPaymentTxnById DFRFSTicketBooking.CONFIRMING updatedTTL (Just txnId.getId) booking.id -- Payment transaction move to frfsticketservice status handler
  let updatedBooking = makeUpdatedBooking booking DFRFSTicketBooking.CONFIRMING (Just updatedTTL) (Just txnId.getId)
  let mRiderName = person.firstName <&> (\fName -> person.lastName & maybe fName (\lName -> fName <> " " <> lName))
  mRiderNumber <- mapM decrypt person.mobileNumber
  void $ QFRFSTicketBooking.insertPayerVpaIfNotPresent paymentStatusResp.payerVpa booking.id
  void $ CallExternalBPP.confirm processOnConfirmReq merchant merchantOperatingCity bapConfig (mRiderName, mRiderNumber) updatedBooking
  pure updatedBooking
  where
    processOnConfirmReq onConfirmReq = do
      (merchant', booking') <- DACFOC.validateRequest onConfirmReq
      DACFOC.onConfirm merchant' booking' onConfirmReq

    makeUpdatedBooking DFRFSTicketBooking.FRFSTicketBooking {..} updatedStatus mTTL transactionId =
      let validTill' = mTTL & fromMaybe validTill
          newPaymentTxnId = transactionId <|> paymentTxnId
       in DFRFSTicketBooking.FRFSTicketBooking {status = updatedStatus, validTill = validTill', paymentTxnId = newPaymentTxnId, ..}
    getSuccessTransactionId transactions = do
      let successTransactions = filter (\transaction -> transaction.status == Payment.CHARGED) transactions
      case successTransactions of
        [] -> throwError $ InvalidRequest "No successful transaction found"
        [transaction] -> return transaction.id
        _ -> throwError $ InvalidRequest "Multiple successful transactions found"
