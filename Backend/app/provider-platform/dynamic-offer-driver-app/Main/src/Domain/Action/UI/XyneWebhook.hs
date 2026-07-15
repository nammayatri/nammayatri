module Domain.Action.UI.XyneWebhook (postXyneWebhook, postXyneBearerWebhook) where

import qualified API.UI.Issue as AUI
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
import Storage.Beam.IssueManagement ()
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as QMSC

postXyneWebhook :: Maybe Text -> RawByteString -> Flow XyneShared.XyneWebhookAck
postXyneWebhook mbSig rawBody = do
  signingSecret <- asks (.xyneWebhookSigningSecret)
  XyneShared.processXyneWebhook
    signingSecret
    lookupXyneCfg
    AUI.driverIssueHandle
    Common.DRIVER
    mbSig
    rawBody

postXyneBearerWebhook :: Maybe Text -> RawByteString -> Flow APISuccess
postXyneBearerWebhook mbAuth rawBody = do
  bearerToken <- asks (.xyneWebhookBearerToken)
  XyneShared.processXyneBearerWebhook bearerToken mbAuth rawBody

lookupXyneCfg ::
  KId.Id Common.Merchant ->
  KId.Id Common.MerchantOperatingCity ->
  Flow Xyne.XyneSpacesCfg
lookupXyneCfg _merchantIdCommon mocIdCommon = do
  let mocId = KId.cast mocIdCommon
  msc <-
    QMSC.findByServiceAndCity (DMSC.IssueTicketService Ticket.XyneSpaces) mocId
      >>= fromMaybeM (InternalError $ "XyneSpaces config not found for mocId=" <> mocIdCommon.getId)
  case msc.serviceConfig of
    DMSC.IssueTicketServiceConfig (Ticket.XyneSpacesConfig cfg) -> pure cfg
    _ -> throwError (InternalError "Unexpected service config shape for XyneSpaces")
