{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.ProviderPlatform.Management.Message
  ( postMessageUploadFile,
    postMessageAddLink,
    postMessageAdd,
    postMessageSend,
    getMessageList,
    getMessageInfo,
    getMessageDeliveryInfo,
    getMessageReceiverList,
    postMessageEdit,
  )
where

import qualified API.Client.ProviderPlatform.Management as Client
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.Message as Common
import qualified Data.Text as DT
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified "lib-dashboard" Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import EulerHS.Prelude
import Kernel.Types.APISuccess (APISuccess)
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (MonadFlow, throwError)
import qualified SharedLogic.Transaction as T
import Storage.Beam.CommonInstances ()
import Storage.Types (FileType (..))
import Tools.Auth.Api
import Tools.Auth.Merchant

buildTransaction ::
  ( MonadFlow m,
    Common.HideSecrets request
  ) =>
  ApiTokenInfo ->
  Maybe request ->
  m DT.Transaction
buildTransaction apiTokenInfo =
  T.buildTransaction (DT.castEndpoint apiTokenInfo.userActionType) (Just DRIVER_OFFER_BPP_MANAGEMENT) (Just apiTokenInfo) Nothing Nothing

postMessageUploadFile :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.UploadFileRequest -> Flow Common.UploadFileResponse
postMessageUploadFile merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  unless (req.fileType `elem` [Audio, Image]) $
    throwError $ InvalidRequest "Only support Audio/Image media type. For Video/MediaLinks use AddLink API."
  transaction <- buildTransaction apiTokenInfo T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (addMultipartBoundary . (.messageDSL.postMessageUploadFile)) req
  where
    addMultipartBoundary clientFn reqBody = clientFn ("XXX00XXX", reqBody)

postMessageAddLink :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.AddLinkAsMedia -> Flow Common.UploadFileResponse
postMessageAddLink merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  unless (((req.fileType == VideoLink || req.fileType == PortraitVideoLink) && checkIfYoutubeLink req.url) || req.fileType == ImageLink) $
    throwError $ InvalidRequest "Only support youtube video links and image links. For Audio use uploadFile API."
  transaction <- buildTransaction apiTokenInfo T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.messageDSL.postMessageAddLink) req
  where
    -- youtube link can be https://youtu.be/shorts/nWbI-DfwRpw or https://www.youtube.com/shorts/nWbI-DfwRpw
    checkIfYoutubeLink link = DT.isPrefixOf "https://" link && DT.isInfixOf "youtu" link

postMessageAdd :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.AddMessageRequest -> Flow Common.AddMessageResponse
postMessageAdd merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  unless (length req.mediaFiles <= 1) $
    throwError $ InvalidRequest "Only support one media file per message. More than one media support will be added soon."
  transaction <- buildTransaction apiTokenInfo T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.messageDSL.postMessageAdd) req

postMessageSend :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.SendMessageRequest -> Flow APISuccess
postMessageSend merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (addMultipartBoundary . (.messageDSL.postMessageSend)) req
  where
    addMultipartBoundary clientFn reqBody = clientFn ("XXX00XXX", reqBody)

getMessageList :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Int -> Maybe Int -> Flow Common.MessageListResponse
getMessageList merchantShortId opCity apiTokenInfo mbLimit mbOffset = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callManagementAPI checkedMerchantId opCity (.messageDSL.getMessageList) mbLimit mbOffset

getMessageInfo :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Message -> Flow Common.MessageInfoResponse
getMessageInfo merchantShortId opCity apiTokenInfo messageId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callManagementAPI checkedMerchantId opCity (.messageDSL.getMessageInfo) messageId

getMessageDeliveryInfo :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Message -> Flow Common.MessageDeliveryInfoResponse
getMessageDeliveryInfo merchantShortId opCity apiTokenInfo messageId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callManagementAPI checkedMerchantId opCity (.messageDSL.getMessageDeliveryInfo) messageId

getMessageReceiverList :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Message -> Maybe Text -> Maybe Common.MessageDeliveryStatus -> Maybe Int -> Maybe Int -> Flow Common.MessageReceiverListResponse
getMessageReceiverList merchantShortId opCity apiTokenInfo messageId number status limit offset = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callManagementAPI checkedMerchantId opCity (.messageDSL.getMessageReceiverList) messageId number status limit offset

postMessageEdit :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.EditMessageRequest -> Flow APISuccess
postMessageEdit merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo T.emptyRequest
  T.withTransactionStoring transaction $ Client.callManagementAPI checkedMerchantId opCity (.messageDSL.postMessageEdit) req
