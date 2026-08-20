module Domain.Action.Beckn.FRFSSeller.Select (handleSelect) where

import qualified Beckn.ACL.FRFSSeller.OnSearch as OnSearchACL
import qualified Beckn.ACL.FRFSSeller.OnSelect as ACL
import qualified BecknV2.FRFS.Enums as SpecEnums
import qualified BecknV2.FRFS.Types as Spec
import qualified BecknV2.OnDemand.Enums as BecknSpec
import Control.Monad.Trans.Except (runExceptT, throwE)
import qualified Data.Text as T
import qualified Domain.Action.Beckn.FRFSSeller.Init as Init
import Environment (Flow)
import qualified ExternalBPP.ExternalAPI.Metro.CMRL.StationList as CMRLStationList
import Kernel.Prelude
import Kernel.Utils.Common
import qualified SharedLogic.FRFSSeller.CallBAP as CallBAP
import qualified SharedLogic.FRFSSeller.Common as Common
import qualified SharedLogic.FRFSSeller.QuoteCache as QuoteCache
import qualified Storage.CachedQueries.BecknConfig as QBC
import qualified Storage.CachedQueries.Merchant as CQM
import Tools.Error

data SelectFailure
  = QuoteUnavailable
  | Unprocessable Text

failureCode :: SelectFailure -> Text
failureCode = \case
  QuoteUnavailable -> "30004"
  Unprocessable _ -> "31001"

failureMessage :: SelectFailure -> Text
failureMessage = \case
  QuoteUnavailable -> "The selected item is no longer available"
  Unprocessable reason -> reason

handleSelect :: Text -> Spec.SelectReq -> Flow ()
handleSelect operator req = do
  let ctx = req.selectReqContext
  bapUriText <-
    ctx.contextBapUri
      & fromMaybeM (InvalidRequest "BapUri missing on select context")
  bapUri <- parseBaseUrl bapUriText
  transactionId <-
    ctx.contextTransactionId
      & fromMaybeM (InvalidRequest "TransactionId missing on select context")
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
  (operatingHours, operatingWindow, stations) <- Init.sellerEnrichment merchantId ctx
  now <- getCurrentTime
  onSelectReq <-
    priceSelection operator transactionId becknConfig.settlementType operatingHours operatingWindow stations req >>= \case
      Right selected -> pure $ ACL.buildOnSelectReq self now ctx selected
      Left failure -> do
        logWarning $ "FRFS seller select rejected: " <> failureMessage failure
        pure $ ACL.buildOnSelectErrorReq self now ctx (failureCode failure) (failureMessage failure)
  CallBAP.sendOnSelect merchantId becknConfig.subscriberId bapUri onSelectReq

priceSelection :: Text -> Text -> Maybe Text -> [(Text, Text)] -> Maybe (UTCTime, UTCTime) -> [CMRLStationList.Station] -> Spec.SelectReq -> Flow (Either SelectFailure ACL.SelectedItem)
priceSelection operator transactionId settlementType operatingHours operatingWindow stations req = runExceptT $ do
  item <-
    (req.selectReqMessage.confirmReqMessageOrder.orderItems >>= listToMaybe)
      & maybe (throwE (Unprocessable "Select carries no item")) pure
  itemId <-
    item.itemId
      & maybe (throwE (Unprocessable "Selected item has no id")) pure
  let quantity =
        fromMaybe 1 $
          item.itemQuantity
            >>= (.itemQuantitySelected)
            >>= (.itemQuantitySelectedCount)
  quote <-
    lift (QuoteCache.findQuote operator transactionId itemId)
      >>= maybe (throwE QuoteUnavailable) pure
  when (quantity < 1 || quantity > quote.maxTicketsPerOrder) $
    throwE (Unprocessable $ "Quantity " <> show quantity <> " outside 1.." <> show quote.maxTicketsPerOrder)
  journeyType <-
    Common.journeyTypeForItemId itemId
      & maybe (throwE (Unprocessable $ "Unrecognised item id " <> itemId)) pure
  unitPrice <-
    readMaybe (T.unpack quote.priceValue)
      & maybe (throwE (Unprocessable $ "Unreadable cached price " <> quote.priceValue)) pure
  pure
    ACL.SelectedItem
      { itemId,
        journeyId = quote.journeyId,
        providerId = quote.providerId,
        providerName = quote.providerName,
        unitPrice = quote.priceValue,
        currency = quote.currency,
        quantity,
        totalPrice = Common.formatPrice (unitPrice * fromIntegral quantity),
        settlementType,
        courtJurisdiction = terms.courtJurisdiction,
        businessTermsUrl = terms.businessTermsUrl,
        maxPaidAreaMinutes = terms.maxPaidAreaMinutes,
        operatingHours,
        operatingWindow,
        cancellationTermsUrl = terms.cancellationTermsUrl,
        journeyTypeCode = journeyType.code,
        journeyTypeName = journeyType.name,
        validityLabel = validity.label,
        validityDuration = validity.duration,
        fromStopCode = quote.fromStopCode,
        fromStopName = quote.fromStopName,
        fromStopLat = fst (Common.stationCoords stations quote.fromStopCode),
        fromStopLon = snd (Common.stationCoords stations quote.fromStopCode),
        toStopCode = quote.toStopCode,
        toStopName = quote.toStopName,
        toStopLat = fst (Common.stationCoords stations quote.toStopCode),
        toStopLon = snd (Common.stationCoords stations quote.toStopCode)
      }
  where
    terms = Common.operatorTerms operator
    validity = Common.ticketValidity operator
