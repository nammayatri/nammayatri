{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}

module IssueManagement.Beckn.ACL.IGM.Utils where

import Control.Lens ((%~))
import Data.Aeson as A
import Data.Text as T
import qualified IGM.Enums as Spec
import qualified IGM.Types as Spec
import qualified IGM.Utils as Utils
import IssueManagement.Common
import Kernel.Prelude
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.TimeRFC339
import Kernel.Utils.Common

mkBapUri :: (HasFlowEnv m r '["nwAddress" ::: BaseUrl]) => Id Merchant -> m BaseUrl
mkBapUri merchantId = asks (.nwAddress) <&> #baseUrlPath %~ (<> "/" <> T.unpack merchantId.getId)

buildContext :: (HasFlowEnv m r '["nwAddress" ::: BaseUrl]) => Spec.Action -> Spec.Domain -> Text -> Merchant -> Text -> Text -> Context.City -> Maybe BapData -> Maybe Text -> m Spec.Context
buildContext action domain bppId merchant txnId msgId city bapData mTTL = do
  now <- UTCTimeRFC3339 <$> getCurrentTime
  bppUrl <- mkBapUri merchant.id
  let contextBapId = bapData <&> (.bapId)
      contextBapUri = bapData <&> (.bapUri)
  cityCode <- getCodeFromCity city
  pure $
    Spec.Context
      { contextVersion = Just "1.0.0",
        contextDomain = encodeToText' domain,
        contextAction = encodeToText' action,
        contextBapId,
        contextBapUri,
        contextBppId = Just bppId,
        contextBppUri = Just $ showBaseUrl bppUrl,
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

buildTTL :: Int -> UTCTime -> Maybe Text
buildTTL ttlInt now = do
  let validTill = addUTCTime (intToNominalDiffTime ttlInt) now
      ttl = diffUTCTime validTill now
  Just $ Utils.durationToText ttl

data BapData = BapData
  { bapId :: T.Text,
    bapUri :: T.Text
  }
  deriving (Show, Eq, Generic)
