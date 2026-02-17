module Domain.Action.UI.TicketKapture (postKaptureCustomerLogin, postKaptureCloseTicket, getGetAllActiveTickets, getGetClosedTicketDetails, getGetClosedTicketIds) where

import qualified API.Types.UI.TicketKapture
import qualified API.Types.UI.TicketKapture as TicketKapture
import Data.List (nubBy)
import Data.Text
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Domain.Types.TicketKapture (TaggedChatMessage (..), TaggedChatMessageContent (..))
import qualified Environment
import EulerHS.Prelude hiding (id)
import IssueManagement.Common (IssueStatus (..))
import qualified IssueManagement.Domain.Types.Issue.IssueChat as ICT
import qualified IssueManagement.Storage.Queries.Issue.IssueChat as QIssueChat
import qualified IssueManagement.Storage.Queries.Issue.IssueReport as QIssueReport
import qualified Kernel.Beam.Functions as B
import qualified Kernel.External.Ticket.Interface.Types as TIT
import qualified Kernel.External.Ticket.Kapture.Types as Kapture
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess as API
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.IssueManagement ()
import qualified Storage.Queries.Person as QPerson
import Tools.Encryption (decryptWithDefault)
import Tools.Ticket

postKaptureCustomerLogin ::
  ( Kernel.Types.Id.Id Domain.Types.Person.Person,
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
  ) ->
  TIT.TicketType ->
  Environment.Flow TicketKapture.TicketKaptureResp
postKaptureCustomerLogin (personId, _, _) ticketType = do
  person <-
    B.runInReplica $
      QPerson.findById personId
        >>= fromMaybeM (PersonNotFound personId.getId)

  mobileNumber <-
    decryptWithDefault person.mobileNumber Nothing
      >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")

  let email = fromMaybe "" person.email

  let firstName = Kernel.Prelude.getField @"firstName" person
      lastName = fromMaybe "" (Kernel.Prelude.getField @"lastName" person)
      fullName =
        case (firstName, lastName) of
          ("", "") -> ""
          ("", l) -> l
          (f, "") -> f
          (f, l) -> f <> " " <> l

  addAndUpdateKaptureCustomerResponse <-
    withTryCatch
      "addAndUpdateKaptureCustomer:kaptureLogin"
      ( addAndUpdateKaptureCustomer
          person.merchantId
          person.merchantOperatingCityId
          ( TIT.KaptureCustomerReq
              personId.getId
              fullName
              mobileNumber
              email
              personId.getId
          )
      )

  kaptureEncryptionResponse <-
    case addAndUpdateKaptureCustomerResponse of
      Right _resp ->
        withTryCatch
          "kaptureEncryption:kaptureLogin"
          ( kaptureEncryption
              person.merchantId
              person.merchantOperatingCityId
              (TIT.KaptureEncryptionReq personId.getId ticketType)
          )
      Left err ->
        throwError $
          InternalError
            ("Add And Update Kapture Customer Ticket API failed - " <> show err)

  case kaptureEncryptionResponse of
    Left err ->
      throwError $
        InternalError
          ("Kapture Encryption API failed - " <> show err)
    Right resp ->
      pure $
        TicketKapture.TicketKaptureResp
          { encryptedCc = resp.encrytedCc,
            encryptedIv = resp.encryptedIv
          }

postKaptureCloseTicket ::
  ( Kernel.Types.Id.Id Domain.Types.Person.Person,
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
  ) ->
  Data.Text.Text ->
  Environment.Flow API.APISuccess
postKaptureCloseTicket (personId, _, _) ticketId = do
  person <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  _ <- updateTicket person.merchantId person.merchantOperatingCityId (TIT.UpdateTicketReq "" ticketId TIT.RS)
  pure API.Success

getGetAllActiveTickets ::
  ( Kernel.Types.Id.Id Domain.Types.Person.Person,
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
  ) ->
  Environment.Flow API.Types.UI.TicketKapture.GetAllActiveTicketsRes
getGetAllActiveTickets (personId, _, _) = do
  person <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  resp <- kapturePullTicket person.merchantId person.merchantOperatingCityId (TIT.KapturePullTicketReq personId.getId "P" "0" "100")
  let activeTickets =
        [ TicketKapture.ActiveTicketsRes
            { rideId =
                case ticketSummary.additionalInfo of
                  Just (TIT.PullAdditionalDetails (Just (TIT.RideIdObject (Just ridText))))
                    | ridText /= "" && ridText /= "null" -> Just (Kernel.Types.Id.Id ridText)
                  _ -> Nothing,
              ticketId = ticketSummary.ticketId
            }
          | ticketSummary <- resp.message,
            ticketSummary.status == "Pending"
        ]
  pure $ TicketKapture.GetAllActiveTicketsRes {TicketKapture.activeTickets = activeTickets}

getGetClosedTicketIds ::
  ( Kernel.Types.Id.Id Domain.Types.Person.Person,
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
  ) ->
  Environment.Flow API.Types.UI.TicketKapture.GetClosedTicketIdsRes
getGetClosedTicketIds (personId, _, _) = do
  let issuePersonId = Kernel.Types.Id.cast personId
  issueChats <- QIssueChat.findAllByPersonId issuePersonId
  closedTicketsResult <- mapM extractClosedTicket issueChats
  let closedTickets = catMaybes closedTicketsResult
      sortedTickets = sortOn (Down . (.updatedAt)) closedTickets
      uniqueClosedTickets = nubBy (\a b -> a.ticketId == b.ticketId) sortedTickets
  pure $ TicketKapture.GetClosedTicketIdsRes {TicketKapture.closedTicketIds = uniqueClosedTickets}
  where
    extractClosedTicket :: ICT.IssueChat -> Environment.Flow (Maybe API.Types.UI.TicketKapture.CloseTicketResp)
    extractClosedTicket issueChat = do
      case issueChat.issueReportId of
        Nothing -> pure Nothing
        Just issueReportId -> do
          mbIssueReport <- QIssueReport.findById issueReportId
          case mbIssueReport of
            Nothing -> pure Nothing
            Just issueReport ->
              if issueReport.status == CLOSED || issueReport.status == RESOLVED
                then
                  pure $
                    Just $
                      TicketKapture.CloseTicketResp
                        { TicketKapture.rideId = Kernel.Types.Id.cast <$> issueChat.rideId,
                          TicketKapture.ticketId = issueChat.ticketId,
                          TicketKapture.updatedAt = issueReport.updatedAt
                        }
                else pure Nothing

transformChatMessageToTagged :: Kapture.ChatMessage -> TaggedChatMessage
transformChatMessageToTagged (Kapture.ChatMessage {..}) =
  TaggedChatMessage
    { chatMessage = transformChatContentToTagged chatMessage,
      senderName = senderName,
      receiverName = receiverName,
      sentDate = sentDate
    }

transformChatContentToTagged :: Kapture.ChatMessageContent -> TaggedChatMessageContent
transformChatContentToTagged (Kapture.TextMessage txt) =
  TextMessage txt
transformChatContentToTagged (Kapture.FileAttachments files) =
  FileAttachments files

getGetClosedTicketDetails ::
  ( Kernel.Types.Id.Id Domain.Types.Person.Person,
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
  ) ->
  Data.Text.Text ->
  Environment.Flow API.Types.UI.TicketKapture.GetClosedTicketDetailsRes
getGetClosedTicketDetails (_personId, _, _) ticketId = do
  mbIssueChat <- QIssueChat.findByTicketId ticketId
  case mbIssueChat of
    Nothing -> do
      pure $ TicketKapture.GetClosedTicketDetailsRes {TicketKapture.chatMessages = []}
    Just issueChat -> do
      case issueChat.kaptureData of
        Nothing -> do
          pure $ TicketKapture.GetClosedTicketDetailsRes {TicketKapture.chatMessages = []}
        Just chatMessages -> do
          let taggedMessages = EulerHS.Prelude.map transformChatMessageToTagged chatMessages
          pure $ TicketKapture.GetClosedTicketDetailsRes {TicketKapture.chatMessages = taggedMessages}
