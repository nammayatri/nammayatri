module Domain.Action.Beckn.FRFSSeller.Init
  ( handleInit,
    sellerEnrichment,
    sellerIntegratedBPPConfig,
    sellerIntegratedBPPConfigForCity,
  )
where

import qualified Beckn.ACL.FRFSSeller.OnInit as ACL
import qualified Beckn.ACL.FRFSSeller.OnSearch as OnSearchACL
import qualified BecknV2.FRFS.Enums as SpecEnums
import qualified BecknV2.FRFS.Types as Spec
import qualified BecknV2.OnDemand.Enums as BecknSpec
import Control.Monad.Trans.Except (runExceptT, throwE)
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Domain.Types.BecknConfig as DBC
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Merchant as DM
import Environment (Flow)
import qualified ExternalBPP.ExternalAPI.CallAPI as ExternalCallAPI
import qualified ExternalBPP.ExternalAPI.Metro.CMRL.StationList as CMRLStationList
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.FRFSSeller.CallBAP as CallBAP
import qualified SharedLogic.FRFSSeller.Common as Common
import qualified SharedLogic.FRFSSeller.QuoteCache as QuoteCache
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified Storage.CachedQueries.BecknConfig as QBC
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Tools.Error

data InitFailure
  = QuoteUnavailable
  | FinderFeeNotAcceptable Text
  | QuantityExceeded Text
  | ItemNotFound Text
  | Unprocessable Text
  | Unserviceable Text

failureCode :: InitFailure -> Common.SellerErrorCode
failureCode = \case
  QuoteUnavailable -> Common.ItemNotFound
  FinderFeeNotAcceptable _ -> Common.FinderFeeNotAcceptable
  QuantityExceeded _ -> Common.ItemQuantityExceeded
  ItemNotFound _ -> Common.ItemNotFound
  Unprocessable _ -> Common.InternalError
  Unserviceable _ -> Common.LocationUnserviceable

failureMessage :: InitFailure -> Text
failureMessage = \case
  QuoteUnavailable -> "The selected item is no longer available"
  FinderFeeNotAcceptable reason -> reason
  QuantityExceeded reason -> reason
  ItemNotFound reason -> reason
  Unprocessable reason -> reason
  Unserviceable reason -> reason

handleInit :: Text -> Spec.InitReq -> Flow ()
handleInit operator req = do
  let ctx = req.initReqContext
  bapUriText <-
    ctx.contextBapUri
      & fromMaybeM (InvalidRequest "BapUri missing on init context")
  bapUri <- parseBaseUrl bapUriText
  transactionId <-
    ctx.contextTransactionId
      & fromMaybeM (InvalidRequest "TransactionId missing on init context")
  merchantId <-
    (.id)
      <$> ( CQM.findByShortId (Common.operatorMerchantShortId operator)
              >>= fromMaybeM (MerchantDoesNotExist operator)
          )
  becknConfig <-
    QBC.findByMerchantIdDomainAndVehicle merchantId (show SpecEnums.FRFS) BecknSpec.METRO
      >>= fromMaybeM (BecknConfigNotFound $ "merchantId:" <> merchantId.getId <> " domain:FRFS vehicle:METRO")
  let self =
        OnSearchACL.SellerIdentity
          { subscriberId = becknConfig.subscriberId,
            subscriberUrl = showBaseUrl becknConfig.subscriberUrl
          }
  (mbOperatorConfig, operatingHours, operatingWindow, stations) <- sellerEnrichment merchantId req.initReqContext
  let cityCode = fromMaybe "" (ctx.contextLocation >>= (.locationCity) >>= (.cityCode))
      serviceability = Common.serviceabilityOf (mbOperatorConfig >>= (.serviceableBapIds)) cityCode ctx.contextBapId
  paymentId <- generateGUID
  now <- getCurrentTime
  onInitReq <-
    initialise serviceability operator transactionId becknConfig mbOperatorConfig paymentId operatingHours operatingWindow stations req >>= \case
      Right order -> pure $ ACL.buildOnInitReq self now ctx order
      Left failure -> do
        logWarning $ "FRFS seller init rejected: " <> failureMessage failure
        pure $ ACL.buildOnInitErrorReq self now ctx (Common.becknError (failureCode failure) (failureMessage failure))
  CallBAP.sendOnInit merchantId becknConfig.subscriberId bapUri onInitReq

initialise :: Common.Serviceability -> Text -> Text -> DBC.BecknConfig -> Maybe Common.OperatorConfig -> Text -> [(Text, Text)] -> Maybe (UTCTime, UTCTime) -> [CMRLStationList.Station] -> Spec.InitReq -> Flow (Either InitFailure ACL.InitialisedOrder)
initialise (Common.Unserviceable reason) _ _ _ _ _ _ _ _ _ = pure (Left (Unserviceable reason))
initialise Common.Serviceable operator transactionId becknConfig mbOperatorConfig paymentId operatingHours operatingWindow stations req =
  initialiseOrder operator transactionId becknConfig mbOperatorConfig paymentId operatingHours operatingWindow stations req

initialiseOrder :: Text -> Text -> DBC.BecknConfig -> Maybe Common.OperatorConfig -> Text -> [(Text, Text)] -> Maybe (UTCTime, UTCTime) -> [CMRLStationList.Station] -> Spec.InitReq -> Flow (Either InitFailure ACL.InitialisedOrder)
initialiseOrder operator transactionId becknConfig mbOperatorConfig paymentId operatingHours operatingWindow stations req = runExceptT $ do
  let order = req.initReqMessage.confirmReqMessageOrder
  item <-
    (order.orderItems >>= listToMaybe)
      & maybe (throwE (Unprocessable "Init carries no item")) pure
  itemId <-
    item.itemId
      & maybe (throwE (Unprocessable "Selected item has no id")) pure
  let quantity =
        fromMaybe 1 $
          item.itemQuantity
            >>= (.itemQuantitySelected)
            >>= (.itemQuantitySelectedCount)
  whenJust (Common.nonZeroBuyerFinderFee order) $ \fee ->
    throwE (FinderFeeNotAcceptable $ "Buyer finder fee must be zero, got " <> fee)
  quote <-
    lift (QuoteCache.findQuote operator transactionId itemId)
      >>= maybe (throwE QuoteUnavailable) pure
  when (quantity < 1 || quantity > quote.maxTicketsPerOrder) $
    throwE (QuantityExceeded $ "Quantity " <> show quantity <> " outside 1.." <> show quote.maxTicketsPerOrder)
  journeyType <-
    Common.journeyTypeForItemId itemId
      & maybe (throwE (ItemNotFound $ "Unrecognised item id " <> itemId)) pure
  unitPrice <-
    readMaybe (T.unpack quote.priceValue)
      & maybe (throwE (Unprocessable $ "Unreadable cached price " <> quote.priceValue)) pure
  let totalPrice = Common.formatPrice (unitPrice * fromIntegral quantity)
  account <- either (throwE . Unprocessable) pure (Common.settlementAccount becknConfig totalPrice)
  cfg <- either (throwE . Unprocessable) pure (Common.operatorConfig mbOperatorConfig)
  lift (QuoteCache.holdQuote cfg.quoteCache.heldTtlSeconds operator transactionId quote)
  let validity = Common.ticketValidity cfg
  pure
    ACL.InitialisedOrder
      { itemId,
        billing = order.orderBilling,
        courtJurisdiction = cfg.courtJurisdiction,
        businessTermsUrl = cfg.businessTermsUrl,
        cancellationTermsUrl = cfg.cancellation.termsUrl,
        operatingWindow,
        fromStopLat = fst (Common.stationCoords stations quote.fromStopCode),
        fromStopLon = snd (Common.stationCoords stations quote.fromStopCode),
        toStopLat = fst (Common.stationCoords stations quote.toStopCode),
        toStopLon = snd (Common.stationCoords stations quote.toStopCode),
        maxPaidAreaMinutes = cfg.maxPaidAreaMinutes,
        operatingHours,
        journeyId = quote.journeyId,
        journeyTypeCode = show journeyType.code,
        journeyTypeName = journeyType.name,
        providerId = quote.providerId,
        providerName = quote.providerName,
        unitPrice = quote.priceValue,
        currency = quote.currency,
        quantity,
        maxTicketsPerOrder = quote.maxTicketsPerOrder,
        totalPrice,
        fromStopCode = quote.fromStopCode,
        fromStopName = quote.fromStopName,
        toStopCode = quote.toStopCode,
        toStopName = quote.toStopName,
        validityLabel = validity.label,
        validityDuration = validity.duration,
        paymentId,
        account
      }

-- | The operator row every seller flow reads its terms and AFCS credentials from. Resolved
-- from the buyer's city, so a context naming a city we do not operate in fails here rather
-- than further down.
sellerIntegratedBPPConfig :: Id DM.Merchant -> Spec.Context -> Flow DIBC.IntegratedBPPConfig
sellerIntegratedBPPConfig merchantId ctx =
  sellerIntegratedBPPConfigForCity merchantId (ctx.contextLocation >>= (.locationCity) >>= (.cityCode))

-- | Same lookup keyed by a bare city code, for the NTS10 settlement contexts, whose shape
-- differs from the TRV11 one and carries the city as a plain field.
sellerIntegratedBPPConfigForCity :: Id DM.Merchant -> Maybe Text -> Flow DIBC.IntegratedBPPConfig
sellerIntegratedBPPConfigForCity merchantId mbCityCode = do
  cityCode <- mbCityCode & fromMaybeM (InvalidRequest "City missing on context")
  city <- case A.fromJSON (A.String cityCode) of
    A.Success c -> pure c
    A.Error e -> throwError (InvalidRequest $ "Unparseable city code " <> cityCode <> ": " <> show e)
  merchantOperatingCity <-
    CQMOC.findByMerchantIdAndCity merchantId city
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant:" <> merchantId.getId <> " city:" <> show city)
  SIBC.findIntegratedBPPConfig Nothing merchantOperatingCity.id BecknSpec.METRO DIBC.MULTIMODAL

sellerEnrichment :: Id DM.Merchant -> Spec.Context -> Flow (Maybe Common.OperatorConfig, [(Text, Text)], Maybe (UTCTime, UTCTime), [CMRLStationList.Station])
sellerEnrichment merchantId ctx = do
  result <- withTryCatch "frfsSeller:init:operatingHours" $ do
    integratedBPPConfig <- sellerIntegratedBPPConfig merchantId ctx
    (,,,) integratedBPPConfig.operatorConfig
      <$> ExternalCallAPI.getOperatingHoursTags integratedBPPConfig
      <*> ExternalCallAPI.getOperatingWindow integratedBPPConfig
      <*> ExternalCallAPI.getStationList integratedBPPConfig
  case result of
    Right enrichment -> pure enrichment
    Left err -> do
      logWarning $ "FRFS seller init: operator enrichment unavailable, publishing without it: " <> show err
      pure (Nothing, [], Nothing, [])
