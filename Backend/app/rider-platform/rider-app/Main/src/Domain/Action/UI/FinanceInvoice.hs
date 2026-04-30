module Domain.Action.UI.FinanceInvoice (getFinanceInvoicePdf) where

import qualified API.Types.UI.FinanceInvoice as API
import qualified Data.Time as DT
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import Environment (Flow)
import EulerHS.Prelude hiding (id)
import Kernel.Prelude (last, listToMaybe)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Finance.Domain.Types.Invoice (InvoiceType)
import Lib.Finance.Invoice.PdfService
import qualified Lib.Finance.Storage.Queries.IndirectTaxTransactionExtra as QIndirectTaxExtra
import qualified Lib.Finance.Storage.Queries.Invoice as QInvoice
import qualified Lib.Finance.Storage.Queries.InvoiceExtra as QInvoiceExtra
import qualified Lib.Payment.Storage.HistoryQueries.PaymentTransaction as HQPaymentTransaction
import Storage.Beam.Payment ()
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.RiderConfig (RiderDimensions (..))
import Storage.ConfigPilot.Interface.Types (getConfig)
import qualified Storage.Queries.Person as QPerson
import Tools.Error
import "beckn-services" Tools.InvoicePdf (generateFinanceInvoicePdf)

getFinanceInvoicePdf ::
  ( Maybe (Id DP.Person),
    Id DM.Merchant
  ) ->
  Maybe DateOrTime ->
  Maybe InvoiceType ->
  Maybe Int ->
  Maybe Int ->
  Maybe Text ->
  Maybe DateOrTime ->
  Flow API.FinanceInvoicePdfResp
getFinanceInvoicePdf (mbPersonId, _) mbFrom mbInvoiceType mbLimit mbOffset mbReferenceId mbTo = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  merchantOpCity <-
    CQMOC.findById person.merchantOperatingCityId
      >>= fromMaybeM (MerchantOperatingCityNotFound person.merchantOperatingCityId.getId)
  mbRiderConfig <- getConfig (RiderDimensions {merchantOperatingCityId = person.merchantOperatingCityId.getId})

  let fromTime = toUTCTimeFrom <$> mbFrom
      toTime = toUTCTimeTo <$> mbTo

  invoices <- case mbReferenceId of
    Just invoiceNumber ->
      QInvoice.findByNumber invoiceNumber >>= \case
        Just inv -> pure [inv]
        Nothing -> pure []
    Nothing ->
      QInvoiceExtra.findByMerchantOpCityIdAndDateRange
        person.merchantOperatingCityId.getId
        fromTime
        toTime
        mbInvoiceType
        Nothing
        (Just personId.getId)
        Nothing
        (if isJust mbFrom || isJust mbTo then Nothing else mbLimit <|> Just 1)
        (mbOffset <|> Just 0)

  when (null invoices) $
    throwError $ InvalidRequest "No invoices found for the given criteria"

  let locale = countryToLocale merchantOpCity.country
      tz = maybe DT.utc (\rc -> DT.minutesToTimeZone (fromIntegral rc.timeDiffFromUtc `div` 60)) mbRiderConfig
      cfg = InvoicePdfConfig {locale, timezone = tz, logoUrl = mbRiderConfig >>= (.invoiceConfig) >>= (.logoUrl)}

  pdfDatas <- forM invoices $ \inv -> do
    let items = parseLineItems inv.lineItems
    taxTxns <- QIndirectTaxExtra.findByInvoiceNumber inv.invoiceNumber
    let mbTaxTxn = Kernel.Prelude.listToMaybe taxTxns
    (mbPayType, mbBrand, mbLast4) <- case inv.paymentOrderId of
      Just orderId -> do
        txns <- HQPaymentTransaction.findAllByOrderId (Id orderId)
        let mbTxn = Kernel.Prelude.listToMaybe txns
        pure (mbTxn >>= (.paymentMethodType), mbTxn >>= (.cardBrand), mbTxn >>= (.cardLastFourDigits))
      Nothing -> pure (Nothing, Nothing, Nothing)
    pure $ buildInvoicePdfData inv items mbTaxTxn mbPayType mbBrand mbLast4

  let lastInv = last invoices
      html = case pdfDatas of
        [single] -> renderInvoiceHtml cfg single
        batch -> renderBatchInvoiceHtml cfg batch

  pdfBase64 <- generateFinanceInvoicePdf lastInv.invoiceNumber html

  pure $
    API.FinanceInvoicePdfResp
      { pdfBase64 = pdfBase64,
        invoiceNumber = lastInv.invoiceNumber
      }

countryToLocale :: Context.Country -> InvoiceLocale
countryToLocale Context.Finland = FI
countryToLocale Context.Netherlands = NL
countryToLocale _ = EN

