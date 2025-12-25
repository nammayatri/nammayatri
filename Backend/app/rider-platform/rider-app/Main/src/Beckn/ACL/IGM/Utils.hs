{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}

module Beckn.ACL.IGM.Utils where

import Control.Lens ((%~))
import Data.Aeson as A
import qualified Data.Text as T
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMop
import Domain.Types.Person
import qualified IGM.Enums as Spec
import qualified IGM.Types as Spec
import qualified IssueManagement.Common.UI.Issue as Common
import qualified IssueManagement.Domain.Types.Issue.IGMIssue as IGMIssueCommon
import Kernel.Prelude
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.TimeRFC339
import Kernel.Utils.Common

mkBapUri :: (HasFlowEnv m r '["nwAddress" ::: BaseUrl]) => Id DM.Merchant -> m BaseUrl
mkBapUri merchantId = asks (.nwAddress) <&> #baseUrlPath %~ (<> "/" <> T.unpack merchantId.getId)

buildContext :: (HasFlowEnv m r '["nwAddress" ::: BaseUrl]) => Spec.Action -> Spec.Domain -> DM.Merchant -> Text -> Text -> Context.City -> Maybe BppData -> Maybe Text -> m Spec.Context
buildContext action domain merchant txnId msgId city bppData mTTL = do
  now <- UTCTimeRFC3339 <$> getCurrentTime
  bapUrl <- mkBapUri merchant.id
  let bapId = merchant.bapId
      contextBppId = bppData <&> (.bppId)
      contextBppUri = bppData <&> (.bppUri)
  cityCode <- getCodeFromCity city
  pure $
    Spec.Context
      { contextVersion = Just "1.0.0",
        contextDomain = encodeToText' domain,
        contextAction = encodeToText' action,
        contextBapId = Just bapId,
        contextBapUri = Just $ showBaseUrl bapUrl,
        contextBppId,
        contextBppUri,
        contextLocation = Just $ tfLocation cityCode,
        contextKey = Nothing,
        contextMessageId = Just msgId,
        contextTimestamp = Just now,
        contextTransactionId = Just txnId,
        contextTtl = mTTL
      }
  where
    getCodeFromCity city_ = do
      let cityCode = toJSON city_
      case cityCode of
        String code -> pure code
        _ -> throwError $ InvalidRequest "Incorrect city"

tfLocation :: Text -> Spec.Location
tfLocation location_code =
  Spec.Location
    { locationCity =
        Just $
          Spec.City
            { cityCode = Just location_code,
              cityName = Nothing
            },
      locationCountry =
        Just $
          Spec.Country
            { countryCode = Just "IND",
              countryName = Nothing
            }
    }

encodeToText' :: (ToJSON a) => a -> Maybe Text
encodeToText' = A.decode . A.encode

mapBecknIssueStatus :: Maybe Common.CustomerResponse -> Maybe Text
mapBecknIssueStatus mbResp =
  case mbResp of
    Just resp | resp == Common.ACCEPT -> Just $ show Spec.CLOSED
    _ -> Just $ show Spec.OPEN

mapBecknIssueType :: Maybe Common.CustomerResponse -> Maybe IGMIssueCommon.IssueType -> Maybe Text
mapBecknIssueType Nothing (Just _) = Nothing -- this case should never happen
mapBecknIssueType Nothing _ = Just $ show Spec.TYPE_ISSUE
mapBecknIssueType (Just Common.ACCEPT) (Just IGMIssueCommon.ISSUE) = Just $ show Spec.TYPE_ISSUE
mapBecknIssueType _ Nothing = Nothing -- this case should never happen as code will already throw error if issue doesnot exist
mapBecknIssueType _ _ = Just $ show Spec.GRIEVANCE

mapDomainIssueStatus :: Maybe Common.CustomerResponse -> IGMIssueCommon.Status
mapDomainIssueStatus mbResp =
  case mbResp of
    Just resp | resp == Common.ACCEPT -> IGMIssueCommon.CLOSED
    Just resp | resp == Common.ESCALATE -> IGMIssueCommon.ESCALATED
    _ -> IGMIssueCommon.OPEN

mapDomainIssueType :: Maybe Common.CustomerResponse -> IGMIssueCommon.IssueType -> IGMIssueCommon.IssueType
mapDomainIssueType Nothing IGMIssueCommon.ISSUE = IGMIssueCommon.ISSUE
mapDomainIssueType (Just Common.ACCEPT) IGMIssueCommon.ISSUE = IGMIssueCommon.ISSUE
mapDomainIssueType _ _ = IGMIssueCommon.GRIEVANCE

timeToText :: UTCTime -> Text
timeToText = T.pack . show

buildIGMIssue :: UTCTimeRFC3339 -> Text -> RideBooking -> Person -> Text -> Spec.Domain -> IGMIssueCommon.IGMIssue
buildIGMIssue now issueId booking rider transactionId domain = do
  IGMIssueCommon.IGMIssue
    { createdAt = convertRFC3339ToUTC now,
      riderId = cast <$> (Just rider.id),
      id = Id issueId,
      issueStatus = IGMIssueCommon.OPEN,
      issueType = IGMIssueCommon.ISSUE,
      respondentAction = Nothing,
      respondentEmail = Nothing,
      respondentName = Nothing,
      respondentPhone = Nothing,
      respondingMerchantId = Just booking.providerId,
      respondentEntityType = Nothing,
      transactionId = transactionId,
      domain = domain,
      merchantOperatingCityId = cast <$> (Just booking.merchantOperatingCityId),
      bookingId = booking.bookingId,
      updatedAt = convertRFC3339ToUTC now,
      customerEmail = Nothing,
      customerName = Nothing,
      customerPhone = Nothing,
      issueRaisedByMerchant = Nothing,
      merchantId = Nothing
    }

updateIGMIssue :: Maybe IGMIssueCommon.IGMIssue -> Maybe Common.CustomerResponse -> UTCTime -> Maybe IGMIssueCommon.IGMIssue
updateIGMIssue Nothing _ _ = Nothing
updateIGMIssue (Just igmIssue) mbResponse now = do
  let updatedIssue =
        igmIssue
          { IGMIssueCommon.issueType = mapDomainIssueType mbResponse igmIssue.issueType,
            IGMIssueCommon.issueStatus = mapDomainIssueStatus mbResponse,
            IGMIssueCommon.updatedAt = now
          }
  Just updatedIssue

mapRating :: Maybe Common.CustomerResponse -> Maybe Common.CustomerRating -> Maybe Text
mapRating (Just Common.ACCEPT) (Just Common.THUMBS_UP) = Just $ show Spec.THUMBS_UP
mapRating (Just Common.ACCEPT) (Just Common.THUMBS_DOWN) = Just $ show Spec.THUMBS_DOWN
mapRating _ _ = Nothing

mapCustomerResponseToAction :: Maybe Common.CustomerResponse -> Text
mapCustomerResponseToAction (Just Common.ACCEPT) = show Spec.CLOSE
mapCustomerResponseToAction (Just Common.ESCALATE) = show Spec.ESCALATE
mapCustomerResponseToAction _ = show Spec.OPEN_ISSUE

data BppData = BppData
  { bppId :: Text,
    bppUri :: Text
  }
  deriving (Show, Eq, Generic)

data RideBooking = RideBooking
  { bookingId :: Text,
    providerId :: Text,
    providerUrl :: BaseUrl,
    merchantOperatingCityId :: Id DMop.MerchantOperatingCity,
    merchantId :: Id DM.Merchant,
    bppItemId :: Text,
    bppBookingId :: Maybe Text,
    status :: Maybe Text,
    contactPhone :: Maybe Text,
    domain :: Spec.Domain,
    quantity :: Maybe Int,
    bppOrderId :: Maybe Text
  }
