module Domain.Action.UI.XyneWebhook (postXyneWebhook, postXyneBearerWebhook) where

import qualified Domain.Action.Dashboard.IssueManagement.Issue as DDIM
import qualified Domain.Types.MerchantServiceConfig as DMSC
import Environment
import EulerHS.Prelude hiding (id)
import qualified IssueManagement.Common as Common
import qualified IssueManagement.Domain.Action.UI.XyneWebhook as XyneShared
import qualified Kernel.External.Ticket.Interface.Types as Ticket
import qualified Kernel.External.Ticket.XyneSpaces.Config as Xyne
import Kernel.External.Ticket.XyneSpaces.Webhook (RawByteString)
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Error
import qualified Kernel.Types.Id as KId
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import Storage.Beam.IssueManagement ()
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import Storage.ConfigPilot.Config.MerchantServiceConfig (MerchantServiceConfigDimensions (..))

postXyneWebhook :: Maybe Text -> RawByteString -> Flow XyneShared.XyneWebhookAck
postXyneWebhook mbSig rawBody = do
  signingSecret <- asks (.xyneWebhookSigningSecret)
  XyneShared.processXyneWebhook
    signingSecret
    lookupXyneCfg
    DDIM.dashboardIssueHandle
    Common.CUSTOMER
    mbSig
    rawBody

postXyneBearerWebhook :: Maybe Text -> RawByteString -> Flow APISuccess
postXyneBearerWebhook mbAuth rawBody = do
  bearerToken <- asks (.xyneWebhookBearerToken)
  XyneShared.processXyneBearerWebhook bearerToken DDIM.dashboardIssueHandle Common.CUSTOMER mbAuth rawBody

lookupXyneCfg ::
  KId.Id Common.Merchant ->
  KId.Id Common.MerchantOperatingCity ->
  Flow Xyne.XyneSpacesCfg
lookupXyneCfg merchantIdCommon mocIdCommon = do
  let merchantId = KId.cast merchantIdCommon
      mocId = KId.cast mocIdCommon
  msc <-
    getOneConfig
      (MerchantServiceConfigDimensions {merchantOperatingCityId = mocId.getId, merchantId = merchantId.getId, serviceName = Just (DMSC.IssueTicketService Ticket.XyneSpaces)})
      (Just (maybeToList <$> CQMSC.findByMerchantOpCityIdAndService merchantId mocId (DMSC.IssueTicketService Ticket.XyneSpaces)))
      >>= fromMaybeM (InternalError $ "XyneSpaces config not found for mocId=" <> mocIdCommon.getId)
  case msc.serviceConfig of
    DMSC.IssueTicketServiceConfig (Ticket.XyneSpacesConfig cfg) -> pure cfg
    _ -> throwError (InternalError "Unexpected service config shape for XyneSpaces")
