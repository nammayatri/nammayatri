{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.IGM.Issue (buildIssueReq) where

import Beckn.ACL.IGM.Utils
import qualified Beckn.ACL.IGM.Utils as Utils
import qualified Domain.Types.Merchant as DM
import Domain.Types.MerchantOperatingCity
import Domain.Types.Person
import qualified IGM.Enums as Spec
import qualified IGM.Types as Spec
import qualified IGM.Utils as Utils
import qualified IssueManagement.Common.UI.Issue as Common
import IssueManagement.Domain.Types.Issue.IGMConfig
import IssueManagement.Domain.Types.Issue.IGMIssue
import IssueManagement.Domain.Types.Issue.IssueCategory as Common
import IssueManagement.Domain.Types.Issue.IssueOption as Common
import Kernel.Prelude
import Kernel.Types.TimeRFC339
import Kernel.Utils.Common

buildIssueReq ::
  (HasFlowEnv m r '["nwAddress" ::: BaseUrl], EncFlow m r, CacheFlow m r, EsqDBFlow m r) =>
  RideBooking ->
  Common.IssueCategory ->
  Maybe Common.IssueOption ->
  Text ->
  DM.Merchant ->
  Person ->
  IGMConfig ->
  MerchantOperatingCity ->
  Maybe Common.CustomerResponse ->
  Maybe Common.CustomerRating ->
  Maybe IGMIssue ->
  m (Spec.IssueReq, Text, IGMIssue)
buildIssueReq ridebookingInfo category option description merchant rider igmConfig merchantOperatingCity mbCustomerAction mbCustomerRating mbIgmIssue = do
  now <- getCurrentTime
  let validTill = addUTCTime (intToNominalDiffTime 30) now
      ttl = diffUTCTime validTill now
      nowRFC3339 = UTCTimeRFC3339 now
  messageId <- generateGUID
  (issueId, mbType, transactionId, createdAt) <- case mbIgmIssue of
    Nothing -> do
      issueId <- generateGUID
      transactionId <- generateGUID
      pure (issueId, Nothing, transactionId, UTCTimeRFC3339 now)
    Just igmIssue -> pure (igmIssue.id.getId, Just igmIssue.issueType, igmIssue.transactionId, UTCTimeRFC3339 igmIssue.createdAt)
  context <- Utils.buildContext Spec.ISSUE ridebookingInfo.domain merchant transactionId messageId merchantOperatingCity.city (Just $ Utils.BppData ridebookingInfo.providerId (showBaseUrl ridebookingInfo.providerUrl)) (Just $ Utils.durationToText ttl)
  let igmIssue = fromMaybe (Utils.buildIGMIssue nowRFC3339 issueId ridebookingInfo rider transactionId ridebookingInfo.domain) (Utils.updateIGMIssue mbIgmIssue mbCustomerAction now)
  pure
    ( Spec.IssueReq
        { context,
          issueReqMessage = tfIssueReqMessage category option description createdAt nowRFC3339 issueId merchant ridebookingInfo rider igmConfig mbCustomerAction mbCustomerRating mbType
        },
      issueId,
      igmIssue
    )

tfIssueReqMessage :: Common.IssueCategory -> Maybe Common.IssueOption -> Text -> UTCTimeRFC3339 -> UTCTimeRFC3339 -> Text -> DM.Merchant -> RideBooking -> Person -> IGMConfig -> Maybe Common.CustomerResponse -> Maybe Common.CustomerRating -> Maybe IssueType -> Spec.IssueReqMessage
tfIssueReqMessage category option description createdAt now issueId merchant ridebookingInfo rider igmConfig mbCustomerAction mbCustomerRating mbType =
  Spec.IssueReqMessage
    { issueReqMessageIssue = tfIssue category option description createdAt now issueId merchant ridebookingInfo rider igmConfig mbCustomerAction mbCustomerRating mbType
    }

tfIssue :: Common.IssueCategory -> Maybe Common.IssueOption -> Text -> UTCTimeRFC3339 -> UTCTimeRFC3339 -> Text -> DM.Merchant -> RideBooking -> Person -> IGMConfig -> Maybe Common.CustomerResponse -> Maybe Common.CustomerRating -> Maybe IssueType -> Spec.Issue
tfIssue category option description createdAt now issueId merchant ridebookingInfo rider igmConfig mbCustomerAction mbCustomerRating mbType = do
  let issueCategory = category.igmCategory
      issueSubCategory = show <$> (option >>= (.igmSubCategory))
      issueType = Utils.mapBecknIssueType mbCustomerAction mbType
      issueStatus = Utils.mapBecknIssueStatus mbCustomerAction
  Spec.Issue
    { issueCategory = issueCategory,
      issueComplainantInfo = tfComplainantInfo ridebookingInfo rider,
      issueCreatedAt = createdAt,
      issueDescription = tfDescription description,
      issueExpectedResolutionTime = Just $ Spec.IssueExpectedResolutionTime (Just $ Utils.computeTtlISO8601 igmConfig.expectedResolutionTime),
      issueExpectedResponseTime = Just $ Spec.IssueExpectedResolutionTime (Just $ Utils.computeTtlISO8601 igmConfig.expectedResponseTime),
      issueId = issueId,
      issueIssueActions = tfIssueActions merchant now igmConfig description mbCustomerAction ridebookingInfo,
      issueIssueType = issueType,
      issueOrderDetails = tfOrderDetails ridebookingInfo,
      issueResolution = Nothing,
      issueResolutionProvider = Nothing,
      issueSource = tfIssueSource merchant,
      issueStatus = issueStatus,
      issueSubCategory = issueSubCategory,
      issueUpdatedAt = now,
      issueRating = Utils.mapRating mbCustomerAction mbCustomerRating
    }

tfDescription :: Text -> Maybe Spec.IssueDescription
tfDescription description = do
  let desc = Just description
  let additionalDesc = tfAdditionalDescription (Just "false") (Just "text/plain")
  Just $ Spec.IssueDescription desc desc additionalDesc

tfAdditionalDescription :: Maybe Text -> Maybe Text -> Maybe Spec.IssueDescriptionAdditionalDesc
tfAdditionalDescription url contentType = do
  Just $ Spec.IssueDescriptionAdditionalDesc url contentType

tfIssueActions :: DM.Merchant -> UTCTimeRFC3339 -> IGMConfig -> Text -> Maybe Common.CustomerResponse -> RideBooking -> Maybe Spec.IssueActions
tfIssueActions merchant now igmConfig description mbCustomerAction ridebookingInfo =
  Just $
    Spec.IssueActions
      { issueActionsRespondentActions = Nothing,
        issueActionsComplainantActions = tfComplainantActions merchant now igmConfig description mbCustomerAction ridebookingInfo
      }

tfComplainantActions :: DM.Merchant -> UTCTimeRFC3339 -> IGMConfig -> Text -> Maybe Common.CustomerResponse -> RideBooking -> Maybe [Spec.ComplainantAction]
tfComplainantActions merchant now igmConfig description mbCustomerAction ridebookingInfo =
  Just
    [ Spec.ComplainantAction
        { complainantActionComplainantAction = Just $ Utils.mapCustomerResponseToAction mbCustomerAction,
          complainantActionShortDesc = Just description,
          complainantActionUpdatedAt = Just now, -- this should be now only sad
          complainantActionUpdatedBy = tfUpdatedBy merchant igmConfig ridebookingInfo
        }
    ]

tfUpdatedBy :: DM.Merchant -> IGMConfig -> RideBooking -> Maybe Spec.Organization
tfUpdatedBy merchant igmConfig ridebookingInfo =
  Just $
    Spec.Organization
      { organizationContact = tfContact igmConfig,
        organizationOrg = tfOrg merchant ridebookingInfo,
        organizationPerson = tfPerson igmConfig
      }

tfOrderDetails :: RideBooking -> Maybe Spec.OrderDetails
tfOrderDetails ridebookingInfo =
  Just $
    Spec.OrderDetails
      { orderDetailsFulfillments = tfFulfillments ridebookingInfo,
        orderDetailsId = ridebookingInfo.bppOrderId <> ridebookingInfo.bppBookingId,
        orderDetailsItems = tfItems ridebookingInfo,
        orderDetailsProviderId = Just $ ridebookingInfo.providerId,
        orderDetailsState = ridebookingInfo.status,
        orderMerchantId = Nothing
      }

tfFulfillments :: RideBooking -> Maybe [Spec.Fulfillment]
tfFulfillments ridebookingInfo =
  Just
    [ Spec.Fulfillment
        { fulfillmentId = Just ridebookingInfo.bppItemId,
          fulfillmentState = ridebookingInfo.status
        }
    ]

tfItems :: RideBooking -> Maybe [Spec.Item]
tfItems ridebookingInfo =
  Just
    [ Spec.Item
        { itemId = Just ridebookingInfo.bppItemId,
          itemQuantity = fmap fromIntegral ridebookingInfo.quantity
        }
    ]

tfIssueSource :: DM.Merchant -> Maybe Spec.IssueSource
tfIssueSource merchant =
  Just $
    Spec.IssueSource
      { issueSourceNetworkParticipantId = Just merchant.bapId,
        issueSourceType = Just $ show Spec.CONSUMER
      }

tfComplainantInfo :: RideBooking -> Person -> Maybe Spec.Complainant
tfComplainantInfo ridebookingInfo rider =
  Just $
    Spec.Complainant
      { complainantContact = tfComplainantContact ridebookingInfo,
        complainantPerson = tfComplainantPerson rider
      }

tfComplainantContact :: RideBooking -> Maybe Spec.Contact
tfComplainantContact ridebookingInfo =
  Just $
    Spec.Contact
      { contactEmail = Just "support@nammayatri.in",
        contactPhone = ridebookingInfo.contactPhone
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

tfOrg :: DM.Merchant -> RideBooking -> Maybe Spec.OrganizationOrg
tfOrg merchant ridebookingInfo =
  Just $
    Spec.OrganizationOrg
      { organizationOrgName = Utils.mkOrgName merchant.bapId ridebookingInfo.domain
      }

tfPerson :: IGMConfig -> Maybe Spec.ComplainantPerson
tfPerson igmConfig =
  Just $
    Spec.ComplainantPerson
      { complainantPersonName = Just igmConfig.groName
      }
