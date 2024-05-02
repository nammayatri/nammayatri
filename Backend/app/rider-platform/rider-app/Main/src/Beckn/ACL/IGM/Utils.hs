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
import Domain.Types.Booking
import qualified Domain.Types.IGMIssue as DIGM
import qualified Domain.Types.Merchant as DM
import Domain.Types.Person
import qualified IGM.Enums as Spec
import qualified IGM.Types as Spec
import qualified IssueManagement.Common.UI.Issue as Common
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

mapIssueStatus :: Maybe Common.CustomerResponse -> Maybe Text
mapIssueStatus mbResp =
  case mbResp of
    Just resp | resp == Common.ACCEPT -> Just $ show Spec.CLOSED
    _ -> Just $ show Spec.OPEN

mapIssueType :: Maybe Common.CustomerResponse -> Maybe DIGM.IssueType -> Maybe Text
mapIssueType (Just Common.ACCEPT) (Just DIGM.ISSUE) = Just $ show Spec.TYPE_ISSUE
mapIssueType _ _ = Just $ show Spec.GRIEVANCE

timeToText :: UTCTime -> Text
timeToText = T.pack . show

buildIGMIssue :: UTCTime -> Text -> Booking -> Person -> Text -> DIGM.IGMIssue
buildIGMIssue now issueId booking rider transactionId = do
  DIGM.IGMIssue
    { createdAt = now,
      riderId = rider.id,
      id = Id issueId,
      issueStatus = DIGM.OPEN,
      issueType = DIGM.ISSUE,
      respondentAction = Nothing,
      respondentEmail = Nothing,
      respondentName = Nothing,
      respondentPhone = Nothing,
      respondingMerchantId = booking.providerId,
      respondentEntityType = Nothing,
      transactionId = transactionId,
      merchantOperatingCityId = booking.merchantOperatingCityId,
      bookingId = booking.id,
      updatedAt = now
    }

data BppData = BppData
  { bppId :: Text,
    bppUri :: Text
  }
  deriving (Show, Eq, Generic)
