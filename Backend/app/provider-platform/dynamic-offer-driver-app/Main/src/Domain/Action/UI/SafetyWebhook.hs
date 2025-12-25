{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.SafetyWebhook where

import Data.Aeson.Types as DAT
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantServiceConfig as DMSC
import Environment
import EulerHS.Prelude
import Kernel.External.Encryption
import Kernel.External.Ticket.Interface.Types as Ticket
import qualified Kernel.External.Verification as Verification
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common hiding (Error)
import Servant hiding (throwError)
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.Cac.MerchantServiceUsageConfig as CQMSUC
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import qualified Tools.Ticket as TT

type SafetyWebhookAPI =
  "safety" :> "suspect" :> "list"
    :> Header "Authorization" Text
    :> ReqBody '[JSON] Value
    :> Post '[JSON] AckResponse

data Suspect = Suspect
  { dl :: Maybe Text,
    voterId :: Maybe Text,
    flaggedCategory :: Text,
    flaggedReason :: Text,
    flaggedBy :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)

newtype SafetyWebhookReq = SafetyWebhookReq
  { suspectList :: [Suspect]
  }
  deriving (Generic, Show, FromJSON, ToJSON)

safetyWebhookHandler ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe Text ->
  Value ->
  Flow AckResponse
safetyWebhookHandler merchantShortId mbOpCity secret val = do
  merchant <- findMerchantByShortId merchantShortId
  merchanOperatingCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just mbOpCity)
  transporterConfig <- SCTC.findByMerchantOpCityId merchanOperatingCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchanOperatingCityId.getId)
  merchantServiceUsageConfig <-
    CQMSUC.findByMerchantOpCityId merchanOperatingCityId Nothing
      >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchanOperatingCityId.getId)
  logDebug $ "runWithServiceConfig: merchantServiceUsageConfig: " <> show merchantServiceUsageConfig
  merchantServiceConfig <-
    CQMSC.findByServiceAndCity (DMSC.DriverBackgroundVerificationService $ (.driverBackgroundVerificationService) merchantServiceUsageConfig) merchanOperatingCityId
      >>= fromMaybeM (InternalError $ "No verification service provider configured for the merchant, merchantOpCityId:" <> merchanOperatingCityId.getId)
  safetyWebhookAuthToken <- case merchantServiceConfig.serviceConfig of
    DMSC.DriverBackgroundVerificationServiceConfig vsc -> do
      case vsc of
        Verification.SafetyPortalConfig cfg -> decrypt $ cfg.safetyWebhookAuthToken
    _ -> throwError $ InternalError "Unknown Service Config"
  unless (secret == Just safetyWebhookAuthToken) $ throwError (InvalidRequest "INVALID_AUTHORIZATION_HEADER")
  let mResp = fromJSON val
  case mResp of
    DAT.Success (resp :: SafetyWebhookReq) -> do
      logInfo $ "Success: " <> show resp
      let description = encodeToText resp.suspectList
      ticket <- TT.createTicket merchant.id merchanOperatingCityId (mkTicket description transporterConfig)
      logInfo $ "Ticket: " <> show ticket
      pure Ack
    DAT.Error err -> do
      logInfo $ "Error 2: " <> show err
      pure Ack
  where
    mkTicket description tConfig =
      Ticket.CreateTicketReq
        { category = "BlackListPortal",
          subCategory = Just "SUSPECTED DRIVER LIST",
          disposition = tConfig.kaptureDisposition,
          issueId = Nothing,
          issueDescription = description,
          mediaFiles = Nothing,
          name = Nothing,
          phoneNo = Nothing,
          personId = "SUSPECTED DRIVER LIST",
          classification = Ticket.DRIVER,
          rideDescription = Nothing,
          queue = tConfig.kaptureQueue,
          becknIssueId = Nothing
        }
