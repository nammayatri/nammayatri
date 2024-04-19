{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.IGM.Issue (buildIssueReq) where

import qualified Beckn.ACL.IGM.Utils as Utils
import Domain.Types.Booking
import Domain.Types.IGMConfig
import Domain.Types.IGMIssue
import qualified Domain.Types.Merchant as DM
import Domain.Types.MerchantOperatingCity
import Domain.Types.Person
import Domain.Types.Ride
import qualified IGM.Enums as Spec
import qualified IGM.Types as Spec
import qualified IGM.Utils as Utils
import qualified IssueManagement.Common.UI.Issue as Common
import Kernel.Prelude
import Kernel.Utils.Common

buildIssueReq ::
  (HasFlowEnv m r '["nwAddress" ::: BaseUrl], EncFlow m r, CacheFlow m r, EsqDBFlow m r) =>
  Booking ->
  Ride ->
  Common.IssueReportReq ->
  DM.Merchant ->
  Person ->
  IGMConfig ->
  MerchantOperatingCity ->
  m (Spec.IssueReq, Text, IGMIssue)
buildIssueReq booking ride issueReport merchant rider igmConfig merchantOperatingCity = do
  now <- getCurrentTime
  let validTill = addUTCTime (intToNominalDiffTime 30) now
      ttl = diffUTCTime validTill now
  transactionId <- generateGUID
  messageId <- generateGUID
  issueId <- generateGUID
  context <- Utils.buildContext Spec.ISSUE Spec.ON_DEMAND merchant transactionId messageId merchantOperatingCity.city (Just $ Utils.BppData booking.providerId (show booking.providerUrl)) (Just $ Utils.durationToText ttl)
  let igmIssue = Utils.buildIGMIssue now issueId booking rider transactionId
  pure $
    ( Spec.IssueReq
        { context,
          issueReqMessage = tfIssueReqMessage issueReport now issueId merchant booking ride rider igmConfig
        },
      issueId,
      igmIssue
    )

tfIssueReqMessage :: Common.IssueReportReq -> UTCTime -> Text -> DM.Merchant -> Booking -> Ride -> Person -> IGMConfig -> Spec.IssueReqMessage
tfIssueReqMessage issueReport now issueId merchant booking ride rider igmConfig =
  Spec.IssueReqMessage
    { issueReqMessageIssue = tfIssue issueReport now issueId merchant booking ride rider igmConfig
    }

tfIssue :: Common.IssueReportReq -> UTCTime -> Text -> DM.Merchant -> Booking -> Ride -> Person -> IGMConfig -> Spec.Issue
tfIssue issueReport now issueId merchant booking ride rider igmConfig = do
  let issueCategory = Utils.mapIssueCategory issueReport.categoryId
  Spec.Issue
    { issueCategory = issueCategory,
      issueComplainantInfo = tfComplainantInfo booking rider,
      issueCreatedAt = now,
      issueDescription = tfDescription issueReport,
      issueExpectedResolutionTime = Just $ Spec.IssueExpectedResolutionTime (Just $ Utils.timeToText igmConfig.expectedResolutionTime),
      issueExpectedResponseTime = Just $ Spec.IssueExpectedResolutionTime (Just $ Utils.timeToText igmConfig.expectedResponseTime),
      issueId = issueId,
      issueIssueActions = tfIssueActions merchant now igmConfig,
      issueIssueType = Just $ show Spec.TYPE_ISSUE,
      issueOrderDetails = tfOrderDetails booking ride,
      issueResolution = Nothing,
      issueResolutionProvider = Nothing,
      issueSource = tfIssueSource merchant,
      issueStatus = Just $ show Spec.OPEN,
      issueSubCategory = Nothing,
      issueUpdatedAt = now,
      issueRating = Nothing
    }

tfDescription :: Common.IssueReportReq -> Maybe Spec.IssueDescription
tfDescription issueReport = Just $ Spec.IssueDescription (Just issueReport.description) (Just issueReport.description)

tfIssueActions :: DM.Merchant -> UTCTime -> IGMConfig -> Maybe Spec.IssueActions
tfIssueActions merchant now igmConfig =
  Just $
    Spec.IssueActions
      { issueActionsRespondentActions = Nothing,
        issueActionsComplainantActions = tfComplainantActions merchant now igmConfig
      }

tfComplainantActions :: DM.Merchant -> UTCTime -> IGMConfig -> Maybe [Spec.ComplainantAction]
tfComplainantActions merchant now igmConfig =
  Just $
    [ Spec.ComplainantAction
        { complainantActionComplainantAction = Just $ show Spec.OPEN_ISSUE,
          complainantActionShortDesc = Nothing,
          complainantActionUpdatedAt = Just now,
          complainantActionUpdatedBy = tfUpdatedBy merchant igmConfig
        }
    ]

tfUpdatedBy :: DM.Merchant -> IGMConfig -> Maybe Spec.Organization
tfUpdatedBy merchant igmConfig =
  Just $
    Spec.Organization
      { organizationContact = tfContact igmConfig,
        organizationOrg = tfOrg merchant,
        organizationPerson = tfPerson igmConfig
      }

tfOrderDetails :: Booking -> Ride -> Maybe Spec.OrderDetails
tfOrderDetails booking ride =
  Just $
    Spec.OrderDetails
      { orderDetailsFulfillments = tfFulfillments booking ride,
        orderDetailsId = Just $ ride.bppRideId.getId,
        orderDetailsItems = tfItems booking,
        orderDetailsProviderId = Just $ booking.providerId,
        orderDetailsState = Just $ show ride.status,
        orderMerchantId = Just $ booking.merchantId.getId
      }

tfFulfillments :: Booking -> Ride -> Maybe [Spec.Fulfillment]
tfFulfillments booking ride =
  Just $
    [ Spec.Fulfillment
        { fulfillmentId = booking.fulfillmentId,
          fulfillmentState = Just $ show ride.status
        }
    ]

tfItems :: Booking -> Maybe [Spec.Item]
tfItems booking =
  Just $
    [ Spec.Item
        { itemId = Just booking.itemId,
          itemQuantity = Nothing
        }
    ]

tfIssueSource :: DM.Merchant -> Maybe Spec.IssueSource
tfIssueSource merchant =
  Just $
    Spec.IssueSource
      { issueSourceNetworkParticipantId = Just merchant.bapId,
        issueSourceType = Just $ show Spec.CONSUMER
      }

tfComplainantInfo :: Booking -> Person -> Maybe Spec.Complainant
tfComplainantInfo booking rider =
  Just $
    Spec.Complainant
      { complainantContact = tfComplainantContact booking,
        complainantPerson = tfComplainantPerson rider
      }

tfComplainantContact :: Booking -> Maybe Spec.Contact
tfComplainantContact booking =
  Just $
    Spec.Contact
      { contactEmail = Nothing,
        contactPhone = Just booking.primaryExophone
      }

tfComplainantPerson :: Person -> Maybe Spec.ComplainantPerson
tfComplainantPerson rider =
  Just $
    Spec.ComplainantPerson
      { complainantPersonName = rider.firstName <> Just " " <> rider.lastName
      }

tfContact :: IGMConfig -> Maybe Spec.GROContact
tfContact igmConfig =
  Just $
    Spec.GROContact
      { gROContactEmail = Just igmConfig.groEmail,
        gROContactPhone = Just igmConfig.groPhone
      }

tfOrg :: DM.Merchant -> Maybe Spec.OrganizationOrg
tfOrg merchant =
  Just $
    Spec.OrganizationOrg
      { organizationOrgName = Just merchant.bapId
      }

tfPerson :: IGMConfig -> Maybe Spec.ComplainantPerson
tfPerson igmConfig =
  Just $
    Spec.ComplainantPerson
      { complainantPersonName = Just igmConfig.groName
      }
