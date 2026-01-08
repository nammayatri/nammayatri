module ExternalBPP.CallAPI.Init where

import qualified Beckn.ACL.FRFS.Init as ACL
import qualified Beckn.ACL.FRFS.Utils as Utils
import qualified Data.Text as T
import Domain.Action.Beckn.FRFS.Common hiding (status)
import qualified Domain.Action.Beckn.FRFS.OnInit as DOnInit
import Domain.Types.BecknConfig
import qualified Domain.Types.FRFSQuoteCategory as DFRFSQuoteCategory
import qualified Domain.Types.FRFSTicketBooking as DBooking
import Domain.Types.IntegratedBPPConfig
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import ExternalBPP.CallAPI.Types
import qualified ExternalBPP.Flow as Flow
import Kernel.Prelude
import Kernel.Utils.Common
import qualified SharedLogic.CallFRFSBPP as CallFRFSBPP
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified SharedLogic.PTCircuitBreaker as CB
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import Tools.Error
import qualified Tools.Metrics as Metrics

init :: (FRFSConfirmFlow m r c) => Merchant -> MerchantOperatingCity -> BecknConfig -> (Maybe Text, Maybe Text) -> DBooking.FRFSTicketBooking -> [DFRFSQuoteCategory.FRFSQuoteCategory] -> Maybe Bool -> m ()
init merchant merchantOperatingCity bapConfig (mRiderName, mRiderNumber) booking quoteCategories mbEnableOffer = do
  Metrics.startMetrics Metrics.INIT_FRFS merchant.name booking.searchId.getId merchantOperatingCity.id.getId
  integratedBPPConfig <- SIBC.findIntegratedBPPConfigFromEntity booking

  -- Circuit breaker check
  let ptMode = CB.vehicleCategoryToPTMode booking.vehicleType
  mRiderConfig <- QRC.findByMerchantOperatingCityId merchantOperatingCity.id Nothing
  let circuitOpen = CB.isCircuitOpen ptMode CB.BookingAPI mRiderConfig
  let cbConfig = CB.parseCircuitBreakerConfig (mRiderConfig >>= (.ptCircuitBreakerConfig))
  let apiConfig = cbConfig.booking

  -- If circuit is open, try canary request
  when circuitOpen $ do
    let canaryAllowed = fromMaybe 1 (apiConfig <&> (.canaryAllowedPerWindow))
    let canaryWindow = fromMaybe 120 (apiConfig <&> (.canaryWindowSeconds))
    canarySlot <- CB.tryAcquireCanarySlot ptMode CB.BookingAPI merchantOperatingCity.id canaryAllowed canaryWindow
    unless canarySlot $ throwError $ PTBookingTemporarilyDisabled (T.pack $ show ptMode)

  case integratedBPPConfig.providerConfig of
    ONDC _ -> do
      providerUrl <- booking.bppSubscriberUrl & parseBaseUrl & fromMaybeM (InvalidRequest "Invalid provider url")
      let categories =
            mapMaybe
              ( \category -> do
                  if category.selectedQuantity > 0
                    then Just $ DCategorySelect {bppItemId = category.bppItemId, quantity = category.selectedQuantity, category = category.category, price = category.offeredPrice}
                    else Nothing
              )
              quoteCategories
      let requestCity = SIBC.resolveOndcCity integratedBPPConfig merchantOperatingCity.city
      bknInitReq <- ACL.buildInitReq (mRiderName, mRiderNumber) booking bapConfig Utils.BppData {bppId = booking.bppSubscriberId, bppUri = booking.bppSubscriberUrl} requestCity categories
      logDebug $ "FRFS InitReq " <> encodeToText bknInitReq
      void $ CallFRFSBPP.init providerUrl bknInitReq merchant.id
    _ -> do
      result <- withTryCatch "ExternalBPP:init" $ Flow.init merchant merchantOperatingCity integratedBPPConfig bapConfig (mRiderName, mRiderNumber) booking quoteCategories
      case result of
        Left err -> do
          throwError $ InternalError $ "Init failed: " <> show err
        Right onInitReq -> do
          processOnInit onInitReq
  where
    processOnInit :: (FRFSConfirmFlow m r c) => DOnInit.DOnInit -> m ()
    processOnInit onInitReq = do
      (merchant', booking', quoteCategories') <- DOnInit.validateRequest onInitReq
      DOnInit.onInit onInitReq merchant' booking' quoteCategories' mbEnableOffer
