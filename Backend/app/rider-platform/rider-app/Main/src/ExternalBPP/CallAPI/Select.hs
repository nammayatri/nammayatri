module ExternalBPP.CallAPI.Select where

import API.Types.UI.FRFSTicketService
import qualified Beckn.ACL.FRFS.Select as ACL
import qualified Beckn.ACL.FRFS.Utils as Utils
import Domain.Action.Beckn.FRFS.Common hiding (status)
import qualified Domain.Action.Beckn.FRFS.OnSelect as DOnSelect
import Domain.Types.BecknConfig
import qualified Domain.Types.FRFSQuote as DQuote
import qualified Domain.Types.FRFSQuoteCategory as DFRFSQuoteCategory
import Domain.Types.IntegratedBPPConfig
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import ExternalBPP.CallAPI.Types
import qualified ExternalBPP.Flow.Select as Flow
import Kernel.Prelude
import Kernel.Utils.Common
import qualified SharedLogic.CallFRFSBPP as CallFRFSBPP
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified Tools.Metrics as Metrics

select ::
  (FRFSConfirmFlow m r c, HasField "blackListedJobs" r [Text]) =>
  Merchant ->
  MerchantOperatingCity ->
  BecknConfig ->
  DQuote.FRFSQuote ->
  [DFRFSQuoteCategory.FRFSQuoteCategory] ->
  Maybe CrisSdkResponse ->
  Maybe Bool ->
  Maybe Bool ->
  m ()
select merchant merchantOperatingCity bapConfig quote quoteCategories crisSdkResponse isSingleMode mbEnableOffer = do
  Metrics.startMetrics Metrics.SELECT_FRFS merchant.name quote.searchId.getId merchantOperatingCity.id.getId
  integratedBPPConfig <- SIBC.findIntegratedBPPConfigFromEntity quote
  case integratedBPPConfig.providerConfig of
    ONDC _ -> do
      fork ("FRFS ONDC SelectReq for " <> show bapConfig.vehicleCategory) $ do
        let categories =
              mapMaybe
                ( \category -> do
                    if category.selectedQuantity > 0
                      then Just $ DCategorySelect {bppItemId = category.bppItemId, quantity = category.selectedQuantity, category = category.category, price = category.price}
                      else Nothing
                )
                quoteCategories
        providerUrl <- quote.bppSubscriberUrl & parseBaseUrl
        let requestCity = SIBC.resolveOndcCity integratedBPPConfig merchantOperatingCity.city
        bknSelectReq <- ACL.buildSelectReq quote bapConfig Utils.BppData {bppId = quote.bppSubscriberId, bppUri = quote.bppSubscriberUrl} requestCity categories
        logDebug $ "FRFS SelectReq " <> encodeToText bknSelectReq
        void $ CallFRFSBPP.select providerUrl bknSelectReq merchant.id
    _ -> do
      onSelectReq <- Flow.select merchant merchantOperatingCity integratedBPPConfig bapConfig quote quoteCategories
      processOnSelect integratedBPPConfig onSelectReq crisSdkResponse isSingleMode mbEnableOffer
  where
    processOnSelect :: (FRFSConfirmFlow m r c, HasField "blackListedJobs" r [Text]) => IntegratedBPPConfig -> DOnSelect -> Maybe CrisSdkResponse -> Maybe Bool -> Maybe Bool -> m ()
    processOnSelect integratedBPPConfig onSelectReq crisSdkResp mbSingleMode enableOffer = do
      (merchant', quote', _) <- DOnSelect.validateRequest onSelectReq
      DOnSelect.onSelect onSelectReq merchant' quote' mbSingleMode enableOffer crisSdkResp integratedBPPConfig
