module Domain.Action.UI.XyneWebhook (postXyneWebhook, postXyneBearerWebhook, getXyneIssues) where

import qualified API.UI.Issue as AUI
import qualified Domain.Types.MerchantServiceConfig as DMSC
import Environment
import EulerHS.Prelude hiding (id)
import qualified IssueManagement.Common as Common
import qualified IssueManagement.Domain.Action.UI.XyneWebhook as XyneShared
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Ticket.Interface.Types as Ticket
import qualified Kernel.External.Ticket.XyneSpaces.Config as Xyne
import qualified Kernel.External.Ticket.XyneSpaces.Types as XyneTypes
import Kernel.External.Ticket.XyneSpaces.Webhook (RawByteString)
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Error
import qualified Kernel.Types.Id as KId
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import Storage.Beam.IssueManagement ()
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.MerchantServiceConfig (MerchantServiceConfigDimensions (..))

postXyneWebhook :: Maybe Text -> Maybe Text -> RawByteString -> Flow XyneShared.XyneWebhookAck
postXyneWebhook mbMocId mbSig rawBody = do
  (mbExpectedMerchantId, usedConfiguredSecret, signingSecret) <- resolveXyneSigningSecret mbMocId
  XyneShared.processXyneWebhook
    signingSecret
    mbExpectedMerchantId
    usedConfiguredSecret
    lookupXyneCfg
    AUI.driverIssueHandle
    Common.DRIVER
    mbSig
    rawBody

postXyneBearerWebhook :: Maybe Text -> RawByteString -> Flow APISuccess
postXyneBearerWebhook mbAuth rawBody = do
  bearerToken <- asks (.xyneWebhookBearerToken)
  XyneShared.processXyneBearerWebhook bearerToken AUI.driverIssueHandle Common.DRIVER mbAuth rawBody

getXyneIssues :: Maybe UTCTime -> Maybe UTCTime -> Maybe Int -> Maybe Int -> Maybe Text -> Flow [XyneTypes.XyneInboundReq]
getXyneIssues mbStartDate mbEndDate mbLimit mbOffset mbAuth = do
  bearerToken <- asks (.xyneWebhookBearerToken)
  XyneShared.fetchXyneIssues bearerToken lookupXyneCfg AUI.driverIssueHandle Common.DRIVER mbStartDate mbEndDate mbLimit mbOffset mbAuth

resolveXyneSigningSecret :: Maybe Text -> Flow (Maybe (KId.Id Common.Merchant), Bool, Text)
resolveXyneSigningSecret Nothing = do
  envSigningSecret <- asks (.xyneWebhookSigningSecret)
  pure (Nothing, False, envSigningSecret)
resolveXyneSigningSecret (Just mocIdText) = do
  let mocId = KId.Id mocIdText
  moc <- CQMOC.findById mocId >>= fromMaybeM (InvalidRequest "INVALID_XYNE_SIGNATURE")
  let merchantIdCommon = KId.cast moc.merchantId
  mbCfg <- lookupXyneCfgMaybe merchantIdCommon (KId.cast mocId)
  envSigningSecret <- asks (.xyneWebhookSigningSecret)
  case mbCfg >>= (.webhookSigningSecret) of
    Just encryptedSecret -> do
      signingSecret <- decrypt encryptedSecret
      pure (Just merchantIdCommon, True, signingSecret)
    Nothing -> pure (Just merchantIdCommon, False, envSigningSecret)

lookupXyneCfgMaybe ::
  KId.Id Common.Merchant ->
  KId.Id Common.MerchantOperatingCity ->
  Flow (Maybe Xyne.XyneSpacesCfg)
lookupXyneCfgMaybe _merchantIdCommon mocIdCommon = do
  let mocId = KId.cast mocIdCommon
  mbMsc <-
    getOneConfig
      (MerchantServiceConfigDimensions {merchantOperatingCityId = mocId.getId, merchantId = Nothing, serviceName = Just (DMSC.IssueTicketService Ticket.XyneSpaces)})
      Nothing
  pure $ case mbMsc <&> (.serviceConfig) of
    Just (DMSC.IssueTicketServiceConfig (Ticket.XyneSpacesConfig cfg)) -> Just cfg
    _ -> Nothing

lookupXyneCfg ::
  KId.Id Common.Merchant ->
  KId.Id Common.MerchantOperatingCity ->
  Flow Xyne.XyneSpacesCfg
lookupXyneCfg merchantIdCommon mocIdCommon =
  lookupXyneCfgMaybe merchantIdCommon mocIdCommon
    >>= fromMaybeM (InternalError $ "XyneSpaces config not found for mocId=" <> mocIdCommon.getId)
