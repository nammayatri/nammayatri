module Domain.Action.Beckn.FRFSSeller.Init
  ( handleInit,
    sellerEnrichment,
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
  | TermsNotAcceptable Text
  | Unprocessable Text

failureCode :: InitFailure -> Text
failureCode = \case
  QuoteUnavailable -> "30004"
  TermsNotAcceptable _ -> "30005"
  Unprocessable _ -> "31001"

failureMessage :: InitFailure -> Text
failureMessage = \case
  QuoteUnavailable -> "The selected item is no longer available"
  TermsNotAcceptable reason -> reason
  Unprocessable reason -> reason

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
  (operatingHours, operatingWindow, stations) <- sellerEnrichment merchantId req.initReqContext
  paymentId <- generateGUID
  now <- getCurrentTime
  onInitReq <-
    initialiseOrder operator transactionId becknConfig paymentId operatingHours operatingWindow stations req >>= \case
      Right order -> pure $ ACL.buildOnInitReq self now ctx order
      Left failure -> do
        logWarning $ "FRFS seller init rejected: " <> failureMessage failure
        pure $ ACL.buildOnInitErrorReq self now ctx (failureCode failure) (failureMessage failure)
  CallBAP.sendOnInit merchantId becknConfig.subscriberId bapUri onInitReq

initialiseOrder :: Text -> Text -> DBC.BecknConfig -> Text -> [(Text, Text)] -> Maybe (UTCTime, UTCTime) -> [CMRLStationList.Station] -> Spec.InitReq -> Flow (Either InitFailure ACL.InitialisedOrder)
initialiseOrder operator transactionId becknConfig paymentId operatingHours operatingWindow stations req = runExceptT $ do
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
    throwE (TermsNotAcceptable $ "Buyer finder fee must be zero, got " <> fee)
  quote <-
    lift (QuoteCache.findQuote operator transactionId itemId)
      >>= maybe (throwE QuoteUnavailable) pure
  when (quantity < 1 || quantity > quote.maxTicketsPerOrder) $
    throwE (TermsNotAcceptable $ "Quantity " <> show quantity <> " outside 1.." <> show quote.maxTicketsPerOrder)
  journeyType <-
    Common.journeyTypeForItemId itemId
      & maybe (throwE (Unprocessable $ "Unrecognised item id " <> itemId)) pure
  unitPrice <-
    readMaybe (T.unpack quote.priceValue)
      & maybe (throwE (Unprocessable $ "Unreadable cached price " <> quote.priceValue)) pure
  let totalPrice = Common.formatPrice (unitPrice * fromIntegral quantity)
  account <- either (throwE . Unprocessable) pure (Common.settlementAccount becknConfig totalPrice)
  lift (QuoteCache.holdQuote operator transactionId quote)
  let validity = Common.ticketValidity operator
  let terms = Common.operatorTerms operator
  pure
    ACL.InitialisedOrder
      { itemId,
        billing = order.orderBilling,
        courtJurisdiction = terms.courtJurisdiction,
        businessTermsUrl = terms.businessTermsUrl,
        cancellationTermsUrl = terms.cancellationTermsUrl,
        operatingWindow,
        fromStopLat = fst (Common.stationCoords stations quote.fromStopCode),
        fromStopLon = snd (Common.stationCoords stations quote.fromStopCode),
        toStopLat = fst (Common.stationCoords stations quote.toStopCode),
        toStopLon = snd (Common.stationCoords stations quote.toStopCode),
        maxPaidAreaMinutes = terms.maxPaidAreaMinutes,
        operatingHours,
        journeyId = quote.journeyId,
        journeyTypeCode = journeyType.code,
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

sellerEnrichment :: Id DM.Merchant -> Spec.Context -> Flow ([(Text, Text)], Maybe (UTCTime, UTCTime), [CMRLStationList.Station])
sellerEnrichment merchantId ctx = do
  result <- withTryCatch "frfsSeller:init:operatingHours" $ do
    cityCode <-
      (ctx.contextLocation >>= (.locationCity) >>= (.cityCode))
        & fromMaybeM (InvalidRequest "City missing on init context")
    city <- case A.fromJSON (A.String cityCode) of
      A.Success c -> pure c
      A.Error e -> throwError (InvalidRequest $ "Unparseable city code " <> cityCode <> ": " <> show e)
    merchantOperatingCity <-
      CQMOC.findByMerchantIdAndCity merchantId city
        >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant:" <> merchantId.getId <> " city:" <> show city)
    integratedBPPConfig <-
      SIBC.findIntegratedBPPConfig Nothing merchantOperatingCity.id BecknSpec.METRO DIBC.MULTIMODAL
    (,,) <$> ExternalCallAPI.getOperatingHoursTags integratedBPPConfig
      <*> ExternalCallAPI.getOperatingWindow integratedBPPConfig
      <*> ExternalCallAPI.getStationList integratedBPPConfig
  case result of
    Right enrichment -> pure enrichment
    Left err -> do
      logWarning $ "FRFS seller init: operator enrichment unavailable, publishing without it: " <> show err
      pure ([], Nothing, [])
