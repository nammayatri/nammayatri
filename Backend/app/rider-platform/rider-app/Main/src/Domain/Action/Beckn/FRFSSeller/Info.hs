module Domain.Action.Beckn.FRFSSeller.Info (handleInfo) where

import qualified Beckn.ACL.FRFSSeller.OnInfo as ACL
import qualified Beckn.ACL.FRFSSeller.OnSearch as OnSearchACL
import qualified BecknV2.FRFS.Enums as SpecEnums
import qualified BecknV2.FRFS.Types as Spec
import qualified BecknV2.OnDemand.Enums as BecknSpec
import qualified Domain.Action.Beckn.FRFSSeller.Init as Init
import Environment (Flow)
import Kernel.Prelude
import Kernel.Utils.Common
import qualified SharedLogic.FRFSSeller.CallBAP as CallBAP
import qualified SharedLogic.FRFSSeller.Common as Common
import qualified Storage.CachedQueries.BecknConfig as QBC
import qualified Storage.CachedQueries.Merchant as CQM
import Tools.Error

handleInfo :: Text -> Spec.InfoReq -> Flow ()
handleInfo operator req = do
  let ctx = req.infoReqContext
  bapUriText <- ctx.contextBapUri & fromMaybeM (InvalidRequest "BapUri missing on info context")
  bapUri <- parseBaseUrl bapUriText
  merchant <-
    CQM.findByShortId (Common.operatorMerchantShortId operator)
      >>= fromMaybeM (MerchantDoesNotExist operator)
  becknConfig <-
    QBC.findByMerchantIdDomainAndVehicle merchant.id (show SpecEnums.FRFS) BecknSpec.METRO
      >>= fromMaybeM (BecknConfigNotFound $ "merchantId:" <> merchant.id.getId <> " domain:FRFS vehicle:METRO")
  integratedBPPConfig <- Init.sellerIntegratedBPPConfig merchant.id ctx
  case ACL.parseSellerEntityInfo (integratedBPPConfig.operatorConfig >>= (.sellerEntityInfo)) of
    Nothing ->
      logWarning $
        "FRFS seller info for " <> operator
          <> ": no usable sellerEntityInfo in operator_config, so no on_info was sent."
          <> " Seed it rather than letting an entity disclosure go out incomplete."
    Just info -> do
      now <- getCurrentTime
      let self =
            OnSearchACL.SellerIdentity
              { subscriberId = becknConfig.subscriberId,
                subscriberUrl = showBaseUrl becknConfig.subscriberUrl
              }
      CallBAP.sendOnInfo merchant.id becknConfig.subscriberId bapUri (ACL.buildOnInfoReq self now ctx info)
