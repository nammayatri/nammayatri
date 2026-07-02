module Domain.Action.UI.XyneWebhook (postXyneWebhook) where

import qualified Domain.Action.Dashboard.IssueManagement.Issue as DDIM
import qualified Domain.Types.MerchantServiceConfig as DMSC
import Environment
import EulerHS.Prelude hiding (id)
import qualified IssueManagement.Common as Common
import qualified IssueManagement.Domain.Action.UI.XyneWebhook as XyneShared
import qualified Kernel.External.Ticket.Interface.Types as Ticket
import qualified Kernel.External.Ticket.XyneSpaces.Config as Xyne
import Kernel.External.Ticket.XyneSpaces.Webhook (RawByteString)
import Kernel.Types.Error
import qualified Kernel.Types.Id as KId
import Kernel.Utils.Common
import Storage.Beam.IssueManagement ()
import Storage.Beam.SystemConfigs ()
import Storage.ConfigPilot.Config.MerchantServiceConfig (MerchantServiceConfigDimensions (..))
import Storage.ConfigPilot.Interface.Types (getOneConfig)

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

lookupXyneCfg ::
  KId.Id Common.Merchant ->
  KId.Id Common.MerchantOperatingCity ->
  Flow Xyne.XyneSpacesCfg
lookupXyneCfg merchantIdCommon mocIdCommon = do
  let merchantId = KId.cast merchantIdCommon
      mocId = KId.cast mocIdCommon
  msc <-
    getOneConfig (MerchantServiceConfigDimensions {merchantOperatingCityId = mocId.getId, merchantId = merchantId.getId, serviceName = Just (DMSC.IssueTicketService Ticket.XyneSpaces)})
      >>= fromMaybeM (InternalError $ "XyneSpaces config not found for mocId=" <> mocIdCommon.getId)
  case msc.serviceConfig of
    DMSC.IssueTicketServiceConfig (Ticket.XyneSpacesConfig cfg) -> pure cfg
    _ -> throwError (InternalError "Unexpected service config shape for XyneSpaces")
