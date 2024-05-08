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
import IssueManagement.Domain.Types.Issue.IssueCategory as Common
import IssueManagement.Domain.Types.Issue.IssueOption as Common
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Types.TimeRFC339
import Kernel.Utils.Common

buildIssueReq ::
  (HasFlowEnv m r '["nwAddress" ::: BaseUrl], EncFlow m r, CacheFlow m r, EsqDBFlow m r) =>
  Booking ->
  Ride ->
  Common.IssueCategory ->
  Maybe (Common.IssueOption) ->
  Text ->
  DM.Merchant ->
  Person ->
  IGMConfig ->
  MerchantOperatingCity ->
  Maybe Common.CustomerResponse ->
  Maybe IGMIssue ->
  m (Spec.IssueReq, Text, IGMIssue)
buildIssueReq booking ride category option description merchant rider igmConfig merchantOperatingCity mbCustomerAction mbIgmIssue = do
  now <- getCurrentTime
  let validTill = addUTCTime (intToNominalDiffTime 30) now
      ttl = diffUTCTime validTill now
      nowRFC3339 = UTCTimeRFC3339 now
  transactionId <- generateGUID
  messageId <- generateGUID
  (issueId, mbType) <- case mbIgmIssue of
    Nothing -> do
      issueId <- generateGUID
      pure (issueId, Nothing)
    Just igmIssue -> pure (igmIssue.id.getId, Just igmIssue.issueType)
  context <- Utils.buildContext Spec.ISSUE Spec.ON_DEMAND merchant transactionId messageId merchantOperatingCity.city (Just $ Utils.BppData booking.providerId (showBaseUrl booking.providerUrl)) (Just $ Utils.durationToText ttl)
  let igmIssue = fromMaybe (Utils.buildIGMIssue nowRFC3339 issueId booking rider transactionId) mbIgmIssue
  pure $
    ( Spec.IssueReq
        { context,
          issueReqMessage = tfIssueReqMessage category option description nowRFC3339 issueId merchant booking ride rider igmConfig mbCustomerAction mbType
        },
      issueId,
      igmIssue
    )

tfIssueReqMessage :: Common.IssueCategory -> Maybe (Common.IssueOption) -> Text -> UTCTimeRFC3339 -> Text -> DM.Merchant -> Booking -> Ride -> Person -> IGMConfig -> Maybe Common.CustomerResponse -> Maybe IssueType -> Spec.IssueReqMessage
tfIssueReqMessage category option description now issueId merchant booking ride rider igmConfig mbCustomerAction mbType =
  Spec.IssueReqMessage
    { issueReqMessageIssue = tfIssue category option description now issueId merchant booking ride rider igmConfig mbCustomerAction mbType
    }

tfIssue :: Common.IssueCategory -> Maybe (Common.IssueOption) -> Text -> UTCTimeRFC3339 -> Text -> DM.Merchant -> Booking -> Ride -> Person -> IGMConfig -> Maybe Common.CustomerResponse -> Maybe IssueType -> Spec.Issue
tfIssue category option description now issueId merchant booking ride rider igmConfig mbCustomerAction mbType = do
  let issueCategory = category.igmCategory
      issueSubCategory = option >>= (.igmSubCategory)
      issueType = Utils.mapIssueType mbCustomerAction mbType
      issueStatus = Utils.mapIssueStatus mbCustomerAction
  Spec.Issue
    { issueCategory = issueCategory,
      issueComplainantInfo = tfComplainantInfo booking rider,
      issueCreatedAt = now,
      issueDescription = tfDescription description,
      issueExpectedResolutionTime = Just $ Spec.IssueExpectedResolutionTime (Just $ Utils.computeTtlISO8601 igmConfig.expectedResolutionTime),
      issueExpectedResponseTime = Just $ Spec.IssueExpectedResolutionTime (Just $ Utils.computeTtlISO8601 igmConfig.expectedResponseTime),
      issueId = issueId,
      issueIssueActions = tfIssueActions merchant now igmConfig,
      issueIssueType = issueType,
      issueOrderDetails = tfOrderDetails booking ride,
      issueResolution = Nothing,
      issueResolutionProvider = Nothing,
      issueSource = tfIssueSource merchant,
      issueStatus = issueStatus,
      issueSubCategory = issueSubCategory,
      issueUpdatedAt = now,
      issueRating = Nothing
    }

tfDescription :: Text -> Maybe Spec.IssueDescription
tfDescription description = Just $ Spec.IssueDescription (Just description) (Just description)

tfIssueActions :: DM.Merchant -> UTCTimeRFC3339 -> IGMConfig -> Maybe Spec.IssueActions
tfIssueActions merchant now igmConfig =
  Just $
    Spec.IssueActions
      { issueActionsRespondentActions = Nothing,
        issueActionsComplainantActions = tfComplainantActions merchant now igmConfig
      }

tfComplainantActions :: DM.Merchant -> UTCTimeRFC3339 -> IGMConfig -> Maybe [Spec.ComplainantAction]
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
      { orderDetailsFulfillments = tfFulfillments ride,
        orderDetailsId = booking.bppBookingId <&> getId,
        orderDetailsItems = tfItems booking,
        orderDetailsProviderId = Just $ booking.providerId,
        orderDetailsState = Just $ show ride.status,
        orderMerchantId = Just $ booking.merchantId.getId
      }

tfFulfillments :: Ride -> Maybe [Spec.Fulfillment]
tfFulfillments ride =
  Just $
    [ Spec.Fulfillment
        { fulfillmentId = Just $ ride.bppRideId.getId,
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
      { organizationOrgName = Just $ merchant.bapId <> "::TRV10" -- shrey00 : make a function for this
      }

tfPerson :: IGMConfig -> Maybe Spec.ComplainantPerson
tfPerson igmConfig =
  Just $
    Spec.ComplainantPerson
      { complainantPersonName = Just igmConfig.groName
      }
