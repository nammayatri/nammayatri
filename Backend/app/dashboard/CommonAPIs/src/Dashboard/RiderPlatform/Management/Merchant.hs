{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Dashboard.RiderPlatform.Management.Merchant
  ( module Dashboard.RiderPlatform.Management.Merchant,
    module Reexport,
  )
where

import API.Types.RiderPlatform.Management.Endpoints.Merchant
import Dashboard.Common.Merchant as Reexport
import Data.Text as T
import Kernel.Prelude
import Kernel.ServantMultipart
import Kernel.Types.Predicate
import qualified Kernel.Utils.Predicates as P
import Kernel.Utils.Validation

---------------------------------------------------------
-- merchant update --------------------------------------

data MerchantUpdateTReq = MerchantUpdateTReq
  { name :: Maybe Text,
    exoPhones :: Maybe (NonEmpty ExophoneReq),
    fcmConfig :: Maybe FCMConfigUpdateTReq,
    gatewayUrl :: Maybe BaseUrl,
    registryUrl :: Maybe BaseUrl
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

validateMerchantUpdateReq :: Validate MerchantUpdateReq
validateMerchantUpdateReq MerchantUpdateReq {..} =
  sequenceA_
    [ validateField "name" name $ InMaybe $ MinLength 3 `And` P.name,
      whenJust exoPhones $ \phones -> do
        sequenceA_
          [ validateField "exoPhones" phones $ UniqueField @"primaryPhone",
            validateField "exoPhones" phones $ UniqueField @"backupPhone"
          ],
      whenJust exoPhones $ \phones -> for_ phones $ \exophoneReq -> do
        validateObject "exoPhones" exophoneReq validateExophoneReq,
      whenJust fcmConfig $ \cfg -> validateObject "fcmConfig" cfg validateFCMConfigUpdateReq
    ]

instance HideSecrets MerchantUpdateReq where
  type ReqWithoutSecrets MerchantUpdateReq = MerchantUpdateTReq
  hideSecrets MerchantUpdateReq {..} =
    MerchantUpdateTReq
      { fcmConfig = hideSecrets <$> fcmConfig,
        ..
      }

---- UpsertTicketConfig ----------------------------------

instance FromMultipart Tmp UpsertTicketConfigReq where
  fromMultipart form = do
    UpsertTicketConfigReq
      <$> fmap fdPayload (lookupFile "file" form)

instance ToMultipart Tmp UpsertTicketConfigReq where
  toMultipart form =
    MultipartData [] [FileData "file" (T.pack form.file) "" (form.file)]

instance FromMultipart Tmp UpsertMerchantPushNotificationCsvReq where
  fromMultipart form =
    UpsertMerchantPushNotificationCsvReq
      <$> fmap fdPayload (lookupFile "file" form)
      <*> lookupInput "merchantOperatingCity" form
      <*> lookupInput "merchantId" form

instance ToMultipart Tmp UpsertMerchantPushNotificationCsvReq where
  toMultipart form =
    MultipartData
      [Input "merchantOperatingCity" form.merchantOperatingCity, Input "merchantId" form.merchantId]
      [FileData "file" (T.pack form.file) "" (form.file)]

instance FromMultipart Tmp UpsertMerchantMessageCsvReq where
  fromMultipart form =
    UpsertMerchantMessageCsvReq
      <$> fmap fdPayload (lookupFile "file" form)
      <*> lookupInput "merchantOperatingCity" form
      <*> lookupInput "merchantId" form

instance ToMultipart Tmp UpsertMerchantMessageCsvReq where
  toMultipart form =
    MultipartData
      [Input "merchantOperatingCity" form.merchantOperatingCity, Input "merchantId" form.merchantId]
      [FileData "file" (T.pack form.file) "" (form.file)]
