{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.TicketKapture (postKaptureCustomerLogin, postKaptureCloseTicket, getGetAllActiveTickets) where

import qualified API.Types.UI.TicketKapture
import qualified API.Types.UI.TicketKapture as TicketKapture
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.Ride
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Beam.Functions as B
import Kernel.External.Encryption
import qualified Kernel.External.Ticket.Interface.Types as TIT
import qualified Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (..))
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant hiding (throwError)
import qualified Storage.Queries.Person as QPerson
import Tools.Auth
import Tools.Ticket

postKaptureCustomerLogin ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    TIT.TicketType ->
    Environment.Flow TicketKapture.TicketKaptureResp
  )
postKaptureCustomerLogin (mbPersonId, _) ticketType = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  mobileNumber <- mapM decrypt person.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")
  email <- fromMaybe "" <$> mapM decrypt person.email
  addAndUpdateKaptureCustomerResponse <-
    try @_ @SomeException
      (addAndUpdateKaptureCustomer person.merchantId person.merchantOperatingCityId (TIT.KaptureCustomerReq personId.getId (fromMaybe "" person.lastName <> fromMaybe "" person.firstName) mobileNumber email personId.getId))

  kaptureEncryptionResponse <-
    case addAndUpdateKaptureCustomerResponse of
      Right _resp ->
        try @_ @SomeException
          (kaptureEncryption person.merchantId person.merchantOperatingCityId (TIT.KaptureEncryptionReq personId.getId ticketType))
      Left err -> throwError $ InternalError ("Add And Update Kapture Customer Ticket API failed - " <> show err)

  case kaptureEncryptionResponse of
    Left err -> throwError $ InternalError ("Kapture Encryption API failed - " <> show err)
    Right resp ->
      return $
        TicketKapture.TicketKaptureResp
          { encryptedCc = resp.encrytedCc,
            encryptedIv = resp.encryptedIv
          }

postKaptureCloseTicket ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Ride.Ride) ->
    Environment.Flow APISuccess
  )
postKaptureCloseTicket (mbPersonId, _) mbRideId = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  resp <- kapturePullTicket person.merchantId person.merchantOperatingCityId (TIT.KapturePullTicketReq personId.getId "P" "0" "100")
  let matchedTicketIds = case mbRideId of
        Nothing ->
          [ ticketSummary.ticketId
            | ticketSummary <- resp.message,
              ticketSummary.status == "Pending",
              case ticketSummary.additionalInfo of
                Just (TIT.PullAdditionalDetails (TIT.RideIdObject mbR)) ->
                  case mbR of
                    Nothing -> True
                    Just t -> t == "" || t == "null"
                _ -> True
          ]
        Just rideId ->
          [ ticketSummary.ticketId
            | ticketSummary <- resp.message,
              ticketSummary.status == "Pending",
              case ticketSummary.additionalInfo of
                Just (TIT.PullAdditionalDetails (TIT.RideIdObject (Just t))) -> t == rideId.getId
                _ -> False
          ]
  forM_ matchedTicketIds $ \ticketId -> do
    updateTicket person.merchantId person.merchantOperatingCityId (TIT.UpdateTicketReq "" ticketId TIT.RS)
  pure Success

getGetAllActiveTickets ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Environment.Flow API.Types.UI.TicketKapture.GetAllActiveTicketsRes
  )
getGetAllActiveTickets (mbPersonId, _) = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  resp <- kapturePullTicket person.merchantId person.merchantOperatingCityId (TIT.KapturePullTicketReq personId.getId "P" "0" "100")
  let activeTickets =
        [ TicketKapture.ActiveTicketsRes
            { rideId =
                case ticketSummary.additionalInfo of
                  Just (TIT.PullAdditionalDetails (TIT.RideIdObject (Just ridText)))
                    | ridText /= "" && ridText /= "null" -> Just (Kernel.Types.Id.Id ridText)
                  _ -> Nothing,
              ticketId = ticketSummary.ticketId
            }
          | ticketSummary <- resp.message,
            ticketSummary.status == "Pending"
        ]
  pure $ TicketKapture.GetAllActiveTicketsRes {TicketKapture.activeTickets = activeTickets}
